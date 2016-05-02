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

	$('#komplexSvar1').click(function(e) {
		
		var correctAnswer = document.getElementById('1b');
		e.preventDefault();

		if (correctAnswer.checked === true) {
			showResult('#rightKomplex1')
		}else {
			showResult('#wrongKomplex1')
		};
	});

	$('#komplexSvar2').click(function(e) {
		
		var correctAnswer = document.getElementById('2b');
		e.preventDefault();

		if (correctAnswer.checked === true) {
			showResult('#rightKomplex2')
		}else {
			showResult('#wrongKomplex2')
		};
	});

	$('#komplexSvar3').click(function(e) {
		
		var correctAnswer = document.getElementById('3b');
		e.preventDefault();

		if (correctAnswer.checked === true) {
			showResult('#rightKomplex3')
		}else {
			showResult('#wrongKomplex3')
		};
	});

	$('#komplexSvar4').click(function(e) {
		
		var correctAnswer = document.getElementById('4c');
		e.preventDefault();

		if (correctAnswer.checked === true) {
			showResult('#rightKomplex4')
		}else {
			showResult('#wrongKomplex4')
		};
	});


	$('#LTISvar2').click(function(e) {

		var answer = $('#LTI2').val();
		e.preventDefault();

		if (answer === "4") {
			showResult('#rightLTI2')
		}else {
			showResult('#wrongLTI2')
		};	
	});

	function showResult(a) {
		$(a).show().delay(200).addClass("in").fadeOut(1500);
	};
});