Shiny.addCustomMessageHandler("updateDetailToggle",
    function(message) {
      // Find button with the specified name
      if(message.val % 2 ==0){
    		$("#" + message.name).html('<i class="fa fa-arrow-down"></i> More details');
      } else{
      		$("#" + message.name).html('<i class="fa fa-arrow-down"></i> Fewer details');
      }
    }
  );

$(document).on('click', '.item', function (e) {
	//alert("Test of button");
	Shiny.onInputChange("clickedWeight", $(this).attr("data-value"))
})
