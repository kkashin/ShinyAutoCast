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

$(document).on('click', '.item', function (e) {
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