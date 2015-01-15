// The JavaScript part of the Org-Edu-HTML project
// (c) 2014 mbork.pl

// The addControls function adds the button (with a class given as a
// parameter) and ok/wrong divs to the currenct object(s).
(function($){
    $.fn.addControls = function(buttonClass) {
	return $(this).append('<div class="controls"></div>').find('div.controls').append(
	    '<button type="button" class=' + buttonClass + '>' + checkName + '</button>',
	    '<span class="ok" style="cursor:default" hidden>' + okName + '</span>',
	    '<span class="wrong" style="cursor:default" hidden>' + wrongName + '</span>'
	);
    };

    $.fn.hideOkWrong = function() {
	return $(this).siblings('span.ok,span.wrong').hide();
    };
    $.fn.showOk = function() {
	return $(this).siblings('span.ok').show();
    };
    $.fn.showWrong = function() {
	return $(this).siblings('span.wrong').show();
    };
})(jQuery);


$( document ).ready(function() {

    // Append a "Check" button and OK/Wrong boxes (divs) to a SCT
    $("fieldset.sct").addControls("sct-check");

    // When the user clicks the "Check" button, check the answers and show appropriate things

    // TODO: on clicking the button, clone the ok or wrong comments
    // and put them together with span.ok or span.wrong in a common div;
    // then show it, and /remove/ it on click.  Also, style it
    // appropriately.

    $('button.sct-check').click(function() {
	if($(this).parent().parent().children('div').children('input:checked').attr('value') == 1) {
	    // If the right answer is checked, hide any OK/Wrong info...
	    $(this).hideOkWrong();
	    $(this).parent().siblings('div').find('div.comment_ok,div.comment_wrong').hide();
	    // ...and show the relevant one.
	    $(this).showOk();
	    $(this).parent().siblings('div').find('div.comment_ok').show();
	} else {
	    // If the wrong answer is checked, do the right thing.
	    $(this).hideOkWrong();
	    $(this).parent().siblings('div').find('div.comment_ok,div.comment_wrong').hide();
	    $(this).showWrong();
	    $(this).parent().siblings('div').find('div.comment_wrong').show();
	};
    });

    // Append the "Check" button and OK/Wrong boxes (divs) to a MCT
    $("fieldset.mct").addControls("mct-check");

    // When the user clicks the "Check" button, check the answers and show appropriate things
    $('button.mct-check').click(function() {
	// Hide possible leftovers from previous trials
	$(this).hideOkWrong();
	$(this).parent().siblings().find('div.comment_ok,div.comment_wrong').hide();

	// Iterate through the answers and check whether all of them are correct.
	// TODO: refactor to use reduce?
    	var somethingIsWrong = false;
    	$(this).parent().siblings('div').children('input').each(function() {
    	    if(($(this).attr('value') == 1) != $(this).is(':checked')) {
		// Wrong answer
    	    	somethingIsWrong = true;
		$(this).parent().find('div.comment_wrong').show();
    	    } else {
		// Good answer
		$(this).parent().find('div.comment_ok').show();
	    }
    	});
	if (somethingIsWrong) {
	    $(this).showWrong();
	} else {
	    $(this).showOk();
	}
    });

    // Append a "Check" button and OK/Wrong boxes (divs) to a cloze
    // test (TODO: buton adding should be abstracted away!)
    $("div.cloze").addControls("cloze-check");

    // When the user clicks the "Check" button, check the answers and show appropriate things
    $('button.cloze-check').click(function() {
	// Hide possible leftovers from previous trials
	$(this).hideOkWrong();

	// Iterate through the answers and check whether all of them are correct.
	// TODO: refactor to use reduce?
    	var somethingIsWrong = false;
    	$(this).parent().parent().find('input[type="text"]').each(function() {
    	    if(!($.inArray($(this).val(),$(this).attr('correct').split('|')) > -1))
	       { somethingIsWrong = true; }
	});
	if (somethingIsWrong) {
	    $(this).showWrong();
	} else {
	    $(this).showOk();
	}
    	});

    // Append a "Check" button and OK/Wrong boxes (divs) to a SCT with "select"
    $("select.sct-sel").parent().addControls("sct-sel-check");

    // When the user clicks the "Check" button, check the answers and show appropriate things
    $('button.sct-sel-check').click(function() {
	if($(this).parent().parent().find('select option:selected').val() == "1") {
	    // If the right answer is checked, hide any OK/Wrong info...
	    $(this).hideOkWrong();
	    // ...and show the relevant one.
	    $(this).showOk().siblings('div.comment_ok').show();
	} else {
	    // If the wrong answer is checked, do the same.
	    $(this).hideOkWrong();
	    $(this).showWrong().siblings('div.comment_wrong').show();
	};
    });

    // Hiding (fading out) the comments about the answers
    $('span.ok,span.wrong').click(function () {
	$(this).fadeOut();
    });
    $('div.comment_ok,div.comment_wrong').hide().click(function () {
	$(this).fadeOut();
	return false; // to prevent bubbling and (un)checking answer by hiding
    });

    // Moving comments outside the labels
    $('span.label > div.comment_ok, span.label > div.comment_wrong').each(function () {
    	$(this).parent().after(this);
    });

    // "Submitting" on pressing Enter
    $('div.cloze input:text,'
      + 'fieldset.sct input:radio,'
      + 'fieldset.mct input:checkbox').keydown(function(event) {
	if(event.which == 13) {
	    $(this).parent().parent().find('div.controls > button').click();
	};
    });
});
