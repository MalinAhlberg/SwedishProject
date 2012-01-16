
var first = "?firstform";
var second = "?text=";
var more = "?more=";


$(function() {
	$.bbq.removeState("page");
	$(window).bind( 'hashchange', function(e) {
		var prevFragment = $.bbq.prevFragment || {};
		
		function hasChanged(key) {
			return prevFragment[key] != e.getState(key);
		}
		if(!hasChanged("page")) return;
		switch(e.getState("page")) {
		case "first":
			$("#result").load(first, function() {
				$("body").attr("class", "first");
				
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
				});
				
				if(e.getState("text")) $("#result").find("textarea").val(e.getState("text"));
				$("#result").find("textarea").change(function() {
					$.bbq.pushState({"text" : $(this).val()});
				});
				$("#result").find("input")
				.click(function() {
					$.bbq.pushState({"page" : "result", "text" : $("#result").find("textarea").val()});
					return false;
				});
			});
			break;
		case "result":
			$("#result").load(second + e.getState("text"),function() {
				$("body").attr("class", "second");
				replaceLinks();
				$(this).find("a").click(function() {
					$.bbq.pushState({"page" : "more"});
					return false;
				});
					
				
			});
			break;
		case "more":

			$("#result").load(more + e.getState("text"), function() {
				$("body").attr("class", "more");
				replaceLinks();
			});
			break;
		}
		$.bbq.prevFragment = $.deparam.fragment();
	});
		
	$.bbq.pushState({"page" : "first"});	
});

function replaceLinks() {
	$("#result").find("a[href$=png]")
	.each(function() {
		$(this).replaceWith($("<img>").attr("src", $(this).attr("href")) );
	});
}
