$(document).keyup(function(event) {
    if ($("#n").is(":focus") && (event.keyCode == 13)) {
        $("#loginbutton").click();
    }
});