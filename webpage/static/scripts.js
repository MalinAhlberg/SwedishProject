
var first = "?firstform";
var second = "?text=";
var more = "?more=";

var pendingRequest = {abort : $.noop};
$.ajaxSetup({
    cache : false
});

$.fn.load_deferred =  function(url, success) {
	var self = this;
	$("#preloader").fadeIn("fast");
	$("#main").animate({opacity : 0}, "fast");
	return $.get(url).done(function(data) {
		self.html(data);
		$.proxy(success, self)();
		$("#preloader").fadeOut("fast");
		$("#main").animate({opacity : 1}, "fast");
	});
};

$(function() {
	
	$("#preloader").spinner({innerRadius: 5, outerRadius: 7, dashes: 8, strokeWidth: 3});
	$("h1").click(function() {
		window.location = "";
	});

	$(window).bind( 'hashchange', function(e) {
		var prevFragment = $.bbq.prevFragment || {};

		function hasChanged(key) {
			return prevFragment[key] != e.getState(key);
		}
	    if(!hasChanged("page")) return;
	    pendingRequest.abort();
	    
		switch(e.getState("page")) {
		case "first":
			pendingRequest = $("#result").load_deferred(first, function() {
				$("body").attr("class", "first");
				
			    if(e.getState("text")) 
			    	$("#result").find("input[name=text]").val(e.getState("text"));
			    var timeout = $.Deferred();


				$("#result").find("input[name=text]").keyup(function() {
				    timeout.reject();
				    timeout = $.Deferred();
				    $.when(timeout).then(function() {
					    $("#completion").load("?input=" + encodeURI($("input[name=text]").val()), function() {});
				    });
			    	setTimeout(timeout.resolve, 500);
				}).focus().val(prevFragment["text"] || "");
				
				
				$("#result").find("input[type=submit]")
				.click(function() {
					$.bbq.pushState({"page" : "result", "text" : $("#result").find("input[name=text]").val()});
					return false;
				});
				
			});
			break;
		case "result":
			pendingRequest = $("#result").load_deferred(second + e.getState("text"),function() {
				$("body").attr("class", "second");
			    $(this).find("p:last a").click(function() {
					$.bbq.pushState({"page" : "more"});
					return false;
				});
			    
			});
			break;
		case "more":

			pendingRequest = $("#result").load_deferred(more + e.getState("text"), function() {
				$("body").attr("class", "more");
			});
			break;
		}
		$.bbq.prevFragment = $.deparam.fragment();
	});
    if($.bbq.getState("page"))
       $(window).trigger("hashchange");	
    else
	$.bbq.pushState({"page" : "first"});	
});
/*
function replaceLinks() {
	$("#result").find("a[href$=svg]")
	.each(function() {
	    $(this).empty();
		$(this).append($("<img>").attr("src", $(this).attr("href")) );
	});
}
*/