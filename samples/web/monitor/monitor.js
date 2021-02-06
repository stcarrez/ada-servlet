
function Refresh(id) {
  jQuery.ajax({
    type: "GET",
	url: "api/monitor/" + id,
	data: null,
	context: document.body,
	success: function(data, status, jqXHDR) {
	    var response = {
	        error: null,
                    message: null,
                    status: jqXHDR.status,
                    contentType: jqXHDR.getResponseHeader('Content-type'),
                    location: jqXHDR.getResponseHeader('Location'),
                    data: data
            };
            var val = [];
            data = data.values;
            for (var i = 0; i < data.length; i++) {
                val.push([i, data[i]]);
            }
            var options = {
                lines: { show: false },
                series: { lines: { show: true, linewidth: 1 }, bars: { show: true, barWidth: 1}, points: { show: true, radius: 0.1 }},
                xaxis: { min: 0, max: 100 },
                yaxis: { min: 0, max: 100 },
            };
            $.plot("#monitor-graph", [ val ], options);
            /* callback(response); */
        }
  });
}

function Add_Value(id, value) {
  jQuery.ajax({
    type: "PUT",
	url: "api/monitor/" + id + "?value=" + value,
	data: "value=" + value,
	context: document.body,
	success: function(data, status, jqXHDR) {
	var response = {
	  error: null,
                    message: null,
                    status: jqXHDR.status,
                    contentType: jqXHDR.getResponseHeader('Content-type'),
                    location: jqXHDR.getResponseHeader('Location'),
                    data: data
                };
      }
    });
    Refresh(id);
}

function Update_Graph() {
  Add_Value(1, Math.floor((Math.random() * 100) + 1));
  setTimeout(function() { Update_Graph(); }, 100);
}

Update_Graph();
