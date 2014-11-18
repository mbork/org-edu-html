// The JavaScript part of the Org-Edu-HTML project
// (c) 2014 mbork.pl

function OkWrongSiblings(element) {
    return element.parent().children().filter('div.ok,div.wrong');
}

$( document ).ready(function() {
    $("fieldset.sct").append(
	$('<button></button>').attr('type','button').text('Check').addClass('sct-check'),
	'<div class="ok" hidden>OK!</div>',
	'<div class="wrong" hidden>Wrong...</div>');
    $('button.sct-check').click(function() {
	if($(this).parent().children('div').children('input:checked').attr('value') == 1) {
	    OkWrongSiblings($(this)).hide();
	    $(this).parent().children().filter('div.ok').show();
	} else {
	    OkWrongSiblings($(this)).hide();
	    $(this).parent().children().filter('div.wrong').show();
	};
    });
});

