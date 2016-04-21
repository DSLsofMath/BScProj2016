$(function() {

	var questions =[{
		question: "What is 1+3?",
		choises: [1, 2, 3, 4, 5],
		correctAnswer: 4
	}];

	var selections = [];

	var introQ = $('#introQ');

	$('#introSvar').click(function(e) {
		
		var correctAnswer = document.getElementById('c');
		e.preventDefault();

		if (correctAnswer.checked === true) {
			$('#successAlert').slideDown();
			$('#successAlert').fadeTo(2000, 500).slideUp(500, function(){
				$('successAlert').alert('close');
			});
		}else {
			$('#falseAlert').slideDown();
			$('#falseAlert').fadeTo(2000, 500).slideUp(500, function(){
				$('falseAlert').alert('close');
			});
		};
	});

});