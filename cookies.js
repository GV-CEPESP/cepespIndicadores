
$(document).ready(function () {
    $('.redBox').draggable();
    $('.blueBox').droppable({
        drop: function (event, ui) {
            var draggable = ui.draggable;
            var droppable = $(this).attr('id');
            //$(this) this is not used
            //check if cookie is set and value is 1
            if(!$.cookie('modal_dismiss')) {
                //moved modal stuff in if case
                $('.modal-body #modalText').append("Assign " + draggable.attr('id').substring(1, 23) + " to " + droppable + "?");
                $("shiny-modal").modal('show');
            }

        }
    });
    //use modal hidden event for checking checkbox status
    $('shiny-modal').on('hidden', function () {
        var status = $("input[name=checkbox-maintenance]", this).is(":checked");
        $.cookie('modal_dismiss', status, {
            expires: 7,
            path: '/'
        });
    });
});