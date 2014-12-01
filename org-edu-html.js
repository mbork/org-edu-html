// The JavaScript part of the Org-Edu-HTML project
// (c) 2014 mbork.pl

$( document ).ready(function() {

    // Append a "Check" button and OK/Wrong boxes (divs) to a SCT
    $("fieldset.sct").append(
	'<button type="button" class="sct-check">Check</button>',
	'<div class="ok" style="cursor:default" hidden>OK!</div>',
	'<div class="wrong" style="cursor:default" hidden>Wrong...</div>');

    // When the user clicks the "Check" button, check the answers and show appropriate things
    $('button.sct-check').click(function() {
	if($(this).parent().children('div').children('input:checked').attr('value') == 1) {
	    // If the right answer is checked, hide any OK/Wrong info...
	    $(this).siblings('div.ok,div.wrong').hide();
	    // ...and show the relevant one.
	    $(this).siblings('div.ok,div.comment_ok').show();
	} else {
	    // If the wrong answer is checked, do the same.
	    $(this).siblings('div.ok,div.wrong').hide();
	    $(this).siblings('div.wrong,div.comment_wrong').show();
	};
    });

    // Append the "Check" button and OK/Wrong boxes (divs) to a MCT
    $("fieldset.mct").append(
	'<button type="button" class="mct-check">Check</button>',
	'<div class="ok" style="cursor:default" hidden>OK!</div>',
	'<div class="wrong" style="cursor:default" hidden>Wrong...</div>');

    // When the user clicks the "Check" button, check the answers and show appropriate things
    $('button.mct-check').click(function() {
	// Hide possible leftovers from previous trials
	$(this).parent().find('div.ok,div.wrong,div.comment_ok,div.comment_wrong').hide();

	// Iterate through the answers and check whether all of them are correct.
	// TODO: refactor to use reduce?
    	var somethingIsWrong = false;
    	$(this).siblings('div').children('input').each(function() {
    	    if(($(this).attr('value') == 1) != $(this).is(':checked')) {
		// Wrong answer
    	    	somethingIsWrong = true;
		$(this).parent().find('div.comment_wrong').show();
    	    } else {
		// Good answer
		$(this).parent().find('div.comment_ok').show();
	    }
    	});
    });

    // Append a "Check" button and OK/Wrong boxes (divs) to a cloze
    // test (TODO: buton adding should be abstracted away!)
    $("div.cloze").append(
	'<button type="button" class="cloze-check">Check</button>',
	'<div class="ok" style="cursor:default" hidden>OK!</div>',
	'<div class="wrong" style="cursor:default" hidden>Wrong...</div>');

    // When the user clicks the "Check" button, check the answers and show appropriate things
    $('button.cloze-check').click(function() {
	// Hide possible leftovers from previous trials
	$(this).parent().find('div.ok,div.wrong,div.comment_ok,div.comment_wrong').hide();

	// Iterate through the answers and check whether all of them are correct.
	// TODO: refactor to use reduce?
    	var somethingIsWrong = false;
    	$(this).parent().find('input[type="text"]').each(function() {
    	    if(!($.inArray($(this).val(),$(this).attr('correct').split('|')) > -1))
	       { somethingIsWrong = true; }
	});
	if (somethingIsWrong) {
	    $(this).parent().find('div.comment_wrong').show();
	} else {
	    $(this).parent().find('div.comment_ok').show();
	}
    	});

    // Append a "Check" button and OK/Wrong boxes (divs) to a SCT with "select"
    $("select.sct-sel").parent().append(
	'<button type="button" class="sct-sel-check">Check</button>',
	'<div class="ok" style="cursor:default" hidden>OK!</div>',
	'<div class="wrong" style="cursor:default" hidden>Wrong...</div>');

    // When the user clicks the "Check" button, check the answers and show appropriate things
    $('button.sct-sel-check').click(function() {
	if($(this).parent().find('select option:selected').val() == "1") {
	    // If the right answer is checked, hide any OK/Wrong info...
	    $(this).siblings('div.ok,div.wrong').hide();
	    // ...and show the relevant one.
	    $(this).siblings('div.ok,div.comment_ok').show();
	} else {
	    // If the wrong answer is checked, do the same.
	    $(this).siblings('div.ok,div.wrong').hide();
	    $(this).siblings('div.wrong,div.comment_wrong').show();
	};
    });

    // Hiding (fading out) the comments about the answers
    $('div.ok,div.wrong').hide().click(function () {
	$(this).fadeOut();
    });
    $('div.comment_ok,div.comment_wrong').hide().click(function () {
	$(this).fadeOut();
	return false; // to prevent bubbling and (un)checking answer by hiding
    });
});
