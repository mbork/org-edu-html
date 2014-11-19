// The JavaScript part of the Org-Edu-HTML project
// (c) 2014 mbork.pl

function OkWrongSiblings(element) {
    return element.parent().children().filter('div.ok,div.wrong');
}

$( document ).ready(function() {
    $("fieldset.sct").append(
	'<button type="button" class="sct-check">Check</button>',
	'<div class="ok" style="cursor:default" hidden>OK!</div>',
	'<div class="wrong style="cursor:default" hidden>Wrong...</div>');
    $('button.sct-check').click(function() {
	if($(this).parent().children('div').children('input:checked').attr('value') == 1) {
	    OkWrongSiblings($(this)).hide();
	    $(this).parent().children().filter('div.ok').show();
	} else {
	    OkWrongSiblings($(this)).hide();
	    $(this).parent().children().filter('div.wrong').show();
	};
    });
    $("fieldset.mct").append(
	'<button type="button" class="mct-check">Check</button>',
	'<div class="ok" style="cursor:default" hidden>OK!</div>',
	'<div class="wrong style="cursor:default" hidden>Wrong...</div>');
    $('button.mct-check').click(function() {
    	var somethingIsWrong = false;
    	$(this).parent().children('div').children('input').each(function() {
    	    if(($(this).attr('value') == 1) != $(this).is(':checked')) {
    	    	somethingIsWrong = true;
    	    }
    	});
    	if(!somethingIsWrong) {
    	    OkWrongSiblings($(this)).hide();
    	    $(this).parent().children().filter('div.ok').show();
    	} else {
    	    OkWrongSiblings($(this)).hide();
    	    $(this).parent().children().filter('div.wrong').show();
    	}
    });
    $('div.ok,div.wrong').click(function () {
	$(this).hide();
    });
});

