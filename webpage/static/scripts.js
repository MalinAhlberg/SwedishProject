
var first = "?firstform";
var second = "?text=";
var more = "?more=";
$.ajaxSetup({
    cache : false
});

$(function() {
	$("h1").click(function() {
		window.location = "";
	});
	//$.bbq.removeState("page");

	$(window).bind( 'hashchange', function(e) {
		var prevFragment = $.bbq.prevFragment || {};

		function hasChanged(key) {
			return prevFragment[key] != e.getState(key);
		}
	    if(!hasChanged("page")) return;
	    var dfd = $.Deferred();
	    dfd.done(function() {
		$("body").animate({opacity : 1}, "fast");

	    });
		$("body").animate({opacity : 0}, "fast");
		switch(e.getState("page")) {
		case "first":
			$("#result").load(first, function() {
				$("body").attr("class", "first");
   				    dfd.resolve();
				/*
				$(this).find("p").last()
				.load("static/explanation.html", function() {
					$(this).find("div div").each(function() {
						var list = $("<ol>").append(
						$.map($(this).text().split(","), function(item, i) {
							return $("<li>").text($.trim(item)).get(0);
						})
						);
						$(this).replaceWith(list);
					});
//				    $("body").animate({opacity : 1}, "fast");
				    dfd.resolve();
				});
				*/
				if(e.getState("text")) $("#result").find("input[name=text]").val(e.getState("text"));
			    var timeout = $.Deferred();


				$("#result").find("input[name=text]").keyup(function() {
				    timeout.reject();
				    timeout = $.Deferred();
			    $.when(timeout).then(function() {
				$.log("timeout done");
				    $("#completion").load("?input=" + encodeURI($("input[name=text]").val()), function() {
				    });
			    });
				    setTimeout(timeout.resolve, 500);
				});
				$("#result").find("input[type=submit]")
				.click(function() {
					$.bbq.pushState({"page" : "result", "text" : $("#result").find("input[name=text]").val()});
					return false;
				});
			});
			break;
		case "result":
			$("#result").load(second + encodeURI(e.getState("text")),function() {
				$("body").attr("class", "second");
			    $(this).find("p:last a").click(function() {
					$.bbq.pushState({"page" : "more"});
					return false;
				});
			    dfd.resolve();
			    
			});
			break;
		case "more":

			$("#result").load(more + encodeURI(e.getState("text")), function() {
				$("body").attr("class", "more");
			    dfd.resolve();
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