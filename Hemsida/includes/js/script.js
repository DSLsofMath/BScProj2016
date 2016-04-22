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

	function showResult(a) {
		$(a).slideDown();
		$(a).fadeTo(2000, 500).slideUp(500, function(){
    		$(a).alert('close');
		});
	};
});