// Define a custom symbol path

$(document).ready(function() {

	Highcharts.SVGRenderer.prototype.symbols.cross = function (x, y, w, h) {
        return ['M', x, y, 'L', x + w, y + h, 'M', x + w, y, 'L', x, y + h, 'z'];
    };
    if (Highcharts.VMLRenderer) {
        Highcharts.VMLRenderer.prototype.symbols.cross = Highcharts.SVGRenderer.prototype.symbols.cross;
    }

var optionsTimeProfile = {
    chart: {
        renderTo: 'timeplot',
        defaultSeriesType: 'column',
        zoomType:"xy",
        style: {
            fontFamily: "Open Sans"
        }
    },
    credits:{
    	enabled: false
    },
    title: {
        text: ''
    },
    xAxis: {
        title: {
            text: 'Time'
        }
    },
    yAxis: {
        title: {
            text: 'Data and Forecasts'
        }
    }, 
    series: [],
    lang: {
    	noData: "Saved weight combinations displayed here."
    },
    noData : {
    	position: {},
    	attr: {},
    	style: {},
	},
	legend: {
		title: {
			text : 'Age  <span style="font-size: 9px; color: #666; font-weight: normal">(Click to hide)</span>'
		}
	},
	tooltip: {
            useHTML: true,
            formatter: function() { 
				var pointType = '<b>' + this.series.options.datatype + '</b><br/>'
				if(this.series.options.datatype=="Forecast"){
					var table = '<table>' +
					'<tr><td>Weights: </td>' + 
                    '<td style="text-align: right">' + this.series.options.weight + '</td></tr>' +
                    '<tr><td>Sigma: </td>' + 
                    '<td style="text-align: right">' + this.series.options.sigmaHa + '</td></tr>' +
                    '<tr><td>Time: </td>' + 
                    '<td style="text-align: right">' + this.x + '</td></tr>' + 
                    '<tr><td>Age: </td>' + 
                    '<td style="text-align: right">' + this.series.options.age + '</td></tr>' +
                    '</table>'
				} else{
					var table = '<table>' +
                    '<tr><td>Time: </td>' + 
                    '<td style="text-align: right">' + this.x + '</td></tr>' + 
                    '<tr><td>Age: </td>' + 
                    '<td style="text-align: right">' + this.series.options.age + '</td></tr>' +
                    '</table>'
				}
			return pointType+table;
			},
        	valueDecimals: 2
        }    
};

var optionsAgeProfile = {
    chart: {
        renderTo: 'ageplot',
        defaultSeriesType: 'column',
        zoomType:"xy"
    },
    credits:{
    	enabled: false
    },
    title: {
        text: ''
    },
    xAxis: {
        title: {
            text: 'Age'
        }
    },
    yAxis: {
        title: {
            text: 'Forecasts'
        }
    }, 
    series: [],
    lang: {
    	noData: "Saved weight combinations displayed here."
    },
    noData : {
    	position: {},
    	attr: {},
    	style: {},
	},
	legend: {
		title: {
			text : 'Time <span style="font-size: 9px; color: #666; font-weight: normal">(Click to hide)</span>'
		}
	},
	tooltip: {
            useHTML: true,
            formatter: function() { 
            	var pointType = '<b>Forecast</b><br/>'
				var table = '<table>' +
					'<tr><td>Weights: </td>' + 
                    '<td style="text-align: right">' + this.series.options.weight + '</td></tr>' +
                    '<tr><td>Sigma: </td>' + 
                    '<td style="text-align: right">' + this.series.options.sigma + '</td></tr>' +
                    '<tr><td>Time: </td>' + 
                    '<td style="text-align: right">' + this.series.options.time + '</td></tr>' + 
                    '<tr><td>Age: </td>' + 
                    '<td style="text-align: right">' + this.x + '</td></tr>' +
                    '</table>'

				return pointType+table;
			},
            valueDecimals: 2
        }        
};

chartTime = new Highcharts.Chart(optionsTimeProfile)
chartAge = new Highcharts.Chart(optionsAgeProfile)


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
  

// fix for slider labels
Shiny.addCustomMessageHandler("selectedWeightPlots",
    function(message) {
    	optionsAgeProfile.series = message.dataAge
    	chartAge = new Highcharts.Chart(optionsAgeProfile)
    	optionsTimeProfile.series = message.dataTime
    	chartTime = new Highcharts.Chart(optionsTimeProfile)
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
});

$(document).on('click', '.selectedWeights .selectize-control.multi .item', function (e) {
	//alert("Test of button");
	// note the RandomReactive forces reactivity even when the clicked weight combination hasn't changed from last time 
	Shiny.onInputChange("clickedWeight", {randomReactive:Math.random(),weight:$(this).attr("data-value"),})
})