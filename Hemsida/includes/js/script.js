$(function() {

	$('#introSvar1').click(function(e) {
		
		var correctAnswer = document.getElementById('1c');
		e.preventDefault();

		if (correctAnswer.checked === true) {
			showResult('#rightIntro1')
		}else {
			showResult('#wrongIntro1')
		};
	});

	$('#introSvar2').click(function(e) {
		
		var correctAnswer = document.getElementById('2a');
		e.preventDefault();

		if (correctAnswer.checked === true) {
			showResult('#rightIntro2')
		}else {
			showResult('#wrongIntro2')
		};
	});

	$('#LTISvar2').click(function(e) {

		var answer = $('#LTI2').val();
		e.preventDefault();

		if (answer === "hej") {
			showResult('#rightLTI2')
		}else {
			showResult('#wrongLTI2')
		};	
	});

	function showResult(a) {
		$(a).show().delay(200).addClass("in").fadeOut(3500);
	};
});