account <- function(input, output, session) {
    # Define an input name for the input which the authenticated previous session_uuid
    # to be signalled to this session. It will written using js.
    cookie_var <- "authenticated_session_uuid"
    cookie_input <- session$ns(cookie_var)

    # Get the cookie using js and write it to input.
    shinyjs::runjs(
        glue("Shiny.onInputChange('{cookie_input}', Cookies.get('{cookie_var}'))")
    )

    hash_oauth_token <- shiny::reactive({
        token <- stringr::str_extract(
            session$clientData$url_hash,
            "(?<=^#access_token=)[A-Z0-9]+"
        )
        
        # Hash persists even after its cleared by updateQueryString. In order
        # to stop it from re authorizing user after logout, we check whether
        # the logout button is pressed.
        if (input$logout == 0 && !is.na(token)) {
            token
        }
    })
    
    putio_user_id <- shiny::reactive({
        # Previous authenticated session uuid that could be used for authentication.
        # This is retrieved from cookie using js.
        authenticated_session_uuid <- input[[cookie_var]]
        
        # If the user is redirected from put.io with authentication, oauth_token will be
        # in the url hash.
        token <- shiny::isolate(hash_oauth_token())
        
        # Flags that are used later in the function
        cookie_authenticated <- FALSE
        user_id <- NULL
        
        if (!is.null(token)) {
            shiny::updateQueryString("#")
            
            account_info <- busboyr::putio_account_info(oauth_token = "3ULDFGUT")
            user_id <- account_info$json$info$user_id
            plan_expiration <- account_info$json$info$plan_expiration_date %>% 
                lubridate::ymd_hms()
            
            shiny::validate(
                shiny::need(
                    plan_expiration > Sys.time(), 
                    message = "You need to have an active put.io account in order to use Busboy."
                )
            )
        } else if (!is.null(authenticated_session_uuid)) {
            cookie_time_constraint <- "NOW() at time zone 'utc' - INTERVAL '7 days'"
            
            user_id <- dplyr::tbl(pool, "session") %>% 
                dplyr::filter(
                    session_uuid == !!authenticated_session_uuid,
                    created_at > sql(cookie_time_constraint)) %>% 
                dplyr::pull(putio_user_id)
            
            shiny::validate(
                shiny::need(
                    length(user_id) == 1,
                    message = "Session expired, please signin again."
                )
            )
            
            cookie_authenticated <- TRUE
        }
        
        shiny::validate(
            shiny::need(
                !is.null(user_id),
                message = "You need to sign in using put.io in order to use Busboy."
            )
        )
        
        pool::poolWithTransaction(pool, function(con) {
            # For some reason, user agent is not available in shiny server. Taking this
            # precaution to prevent an error in insert_row.
            user_agent <- session$request$HTTP_USER_AGENT
            if (is.null(user_agent)) user_agent <- NA_character_
            
            session_uuid <- busboyr::insert_row(con, "session", list(
                user_agent = user_agent,
                ip_address = session$request$REMOTE_ADDR,
                putio_user_id = user_id
            ), returning = "session_uuid")
            
            # If the user didn't authenticate using a cookie already, set it so that
            # it can be used for authentication later.
            if (!cookie_authenticated) {
                shinyjs::runjs(
                    glue("Cookies.set('{cookie_var}', 
                                      '{session_uuid}', {{ expires: 7 }})")
                )
            }

            sqs_name <- glue("session-{session_uuid}")
            
            # Create aws sqs queue for the session
            queue_url <- aws.sqs::create_queue(sqs_name, query = list(
                Attribute.1.Name = "ReceiveMessageWaitTimeSeconds", # long poll
                Attribute.1.Value = 20
            ))
            
            aws.sqs::set_queue_attrs(sqs_name, list(
                Policy = busboyr::sqs_public_policy(queue_url)
            ))

            shinyjs::runjs(glue("sqs_long_poll('{queue_url}')"))
            
            session$onSessionEnded(function() {
                # Write session end date when session ends
                statement <- glue(
                    "UPDATE session SET ended_at = timezone('utc'::text, now()) 
                     WHERE session_uuid = '{session_uuid}'")
                DBI::dbExecute(pool, statement)
                
                # Close the sqs queue created for the session
                aws.sqs::delete_queue(sqs_name)
            })
        })
        
        bit64::as.integer64(user_id)
    })
    
    putio_user <- shiny::reactive({
        busboyr::putio_cached_user(putio_user_id())
    })
    
    shiny::observeEvent(input$logout, {
        shinyjs::hide(selector = "#putio_user_information")
        shinyjs::runjs(glue("
            Cookies.remove('{cookie_var}');
            Shiny.onInputChange('{cookie_input}', null)
        "))
    })
    
    shiny::observeEvent(input$signin, {
        putio_auth_url <- busboyr::putio_oauth_redirect_url(session$request$HTTP_ORIGIN)
        shinyjs::runjs(glue("window.location = '{putio_auth_url}'"))
    })
    
    shiny::observe({
        tryCatch(
            putio_user_id(),
            error = function(e) {
                shinyjs::show(selector = "#signin_with_putio")
                stop(e)
            }
        )

        shinyjs::hide(selector = "#signin_with_putio")
        shinyjs::show(selector = "#putio_user_information")
    })

    output$putio_username <- shiny::renderText(putio_user()$username)
    output$putio_email <- shiny::renderText(putio_user()$mail)
    output$putio_disk_space <- shiny::renderText({
        available <- utils:::format.object_size(putio_user()$disk$avail, "auto")
        total <- utils:::format.object_size(putio_user()$disk$size, "auto")
        glue("{available} of {total} remaining")
    })
    
    output$support_email <- shiny::renderUI({
        shiny::tagList(
            shiny::tags$span("For questions, email: "),
            shiny::tags$a(href = "mailto:info@busboy.io", "info@busboy.io")
        )
    })
    
    putio_user_id
}
