function limit_session_time(duration) {
    setTimeout(function() {
        window.close();
    }, duration);
}

function setup_search_bar(query_input, button_input) {
    query_element = $("#" + query_input);
    query_element.prop("autofocus", true);
    query_element.focus();
    query_element.keydown(function(event) {
        if (event.which == 13) {
            button_element = $("#" + button_input);
            button_element.click();
        }
    });
}

function sqs_long_poll(queue_url) {
    console.log("Long polling sqs queue...");
    
    // This will be passed as a callback function to
    // jquery ajax
    success_callback = function () {
        sqs_long_poll(queue_url);
    };
    
    receive_url = queue_url + "?Action=ReceiveMessage";
    $.ajax({url: receive_url, success: function(data) {
        receipt_handle = $(data).find("ReceiptHandle").text();
        if (receipt_handle !== "") {
            receipt_handle = encodeURIComponent(receipt_handle);
            message = $(data).find("Message Body").text();
            
            console.log("Got sqs message:", message);
            
            // Killswitch. Could be used for maintenance.
            if (message == "kill") {
                window.close();
            } else {
                // Triggering the shiny input sent from the message
                time = (new Date()).getTime();
                Shiny.onInputChange("refresh", message + "|" + time);
                console.log("Triggered shiny input");
            }
            
            delete_url = queue_url + "?Action=DeleteMessage&" +
                         "ReceiptHandle=" + receipt_handle;
                         
            console.log("Deleting last received message");
            
            $.ajax({url: delete_url, success: success_callback});
        } else {
            success_callback();
        }
    }, dataType: "xml"});
}

function setup_input_button() {
    $('body').on('click', '.input-button', function() {
        // Send custom data value to shiny
        input_name = $(this).data('input-name');
        input_value = $(this).data('input-value');
        Shiny.onInputChange(input_name, input_value);
    });
}
