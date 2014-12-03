Shiny.addCustomMessageHandler("updateDetailToggle",
    function(message) {
      // Find button with the specified name
      if(message.val % 2 ==0){
    		$("#" + message.name).html('<i class="fa fa-level-down"></i> More details');
      } else{
      		$("#" + message.name).html('<i class="fa fa-level-up"></i> Fewer details');
      }
    }
  );

$(document).on('click', '.selectize-control.multi .item', function (e) {
	//alert("Test of button");
	// note the RandomReactive forces reactivity even when the clicked weight combination hasn't changed from last time 
	Shiny.onInputChange("clickedWeight", {randomReactive:Math.random(),weight:$(this).attr("data-value"),})
})

// fix for slider labels
Shiny.addCustomMessageHandler("updateSliderLabels",
    function(message) {
		$(".jslider-label").html("<span>Fit</span>");
	$(".jslider-label-to").html("<span>Smoothness</span>");
    }
);

// disable previous & next buttons if at beginning or end of list
Shiny.addCustomMessageHandler("disableNav",
    function(message) {
      if(message.prevDisable){
      	$("#prevButton").prop('disabled',true);
      } else{
      	$("#prevButton").prop('disabled',null);
      }
      if(message.nextDisable){
      	$("#nextButton").prop('disabled',true);
      } else{
      	$("#nextButton").prop('disabled',null);
      }      
    }
  );
  



	
/* fix for height */
/*$(".well .row-fluid").height()*/
/*$(document).ready(function() {
	$('.pushDown').each(function() {
	    console.log($(this).closest('.row-fluid').height())
	    console.log($(this).height())
		$(this).css('margin-top',$(this).closest('.row-fluid').height()-$(this).height())
	});
});
*/