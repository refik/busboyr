#' Convert sqs url to resource identifier
#' 
#' @export
sqs_url_to_arn <- function(queue_url) {
    stringr::str_extract(queue_url, "sqs.+") %>% 
        stringr::str_replace(".amazonaws.com", "") %>% 
        stringr::str_replace_all("[\\.\\/]", ":") %>% 
        paste0("arn:aws:", .)
}

#' Public policy for sqs queue
#' 
#' @export
sqs_public_policy <- function(queue_url) {
    glue('
        {{
            "Version": "2012-10-17",
            "Id": "{random_string()}",
            "Statement": [
                {{
                    "Sid": "1",
                    "Effect": "Allow",
                    "Principal": "*",
                    "Action": [
                        "SQS:ReceiveMessage",
                        "SQS:DeleteMessage"
                    ],
                    "Resource": "{sqs_url_to_arn(queue_url)}"
                }}
            ]
        }}
    ')
}