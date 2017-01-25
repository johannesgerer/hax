/*
http://datatables.net/reference/option/
*/

function sep(e)
{
		$("table *:nth-child("+e+") ").css("border-right","1px solid black");
}
function hsep(e)
{
		// hsep(1);
		// for(var i in data.dates){
		// 		if(/12\/.*/.test(data.dates[i])){
		// 				hsep(parseInt(i)+2);
		// 		}
		// }
		$("table tr:nth-child("+e+") td,th").css("border-bottom","1px solid black");
}

$(document).ready(function() {
		keys();
		$('.regular').css("background","#eeeeee");
		$('body').css({"font-size":"12px",
									"margin":"0px"});
		$('#data').addClass("cell-border display compact");
		$('#data').css({"text-align" : "right"});
		var i = 1;
		sep(i);
		for (var key in data.entities){
				i += data.entities[key][1].length;
				sep(i);
		}
		$(".regular.endOfYear td,th").css("border-bottom","1px solid black");
    $('#data').dataTable({
		});
		
		if(false){ //alternative: generate dynamically using JSON data
				$('#example').addClass("cell-border display compact");
				
				var header = data.accounts.map(function(e){
						return {"title":e.fEntity +"\n"+ e.fAccount};
				});
				
				$('#example').dataTable( 
						{"data": data.balances
						 ,"columns": header
						 , "paging": false
						 , "ordering": false,
						} );    
		}

} );


state = { transaction : true,
					comment : true,
					duringYear : true,
					Innk: true,
				  Mo: true};

function keys()
{
		function s(a){
				if(state[a])
						$('.'+a).show();
		}
		function h(a){
				if(!state[a])
						$('.'+a).hide();
		}
		function apply(a){
				h(a);s(a);
		}
		function tog(a){
				state[a]=!state[a];
				s('duringYear');
				s('comment');
				s('transaction');
				apply('Mo');
				apply('Innk');
				h('transaction');
				h('comment');
				h('duringYear');
		}

$(document).keypress(function(e) {
		// console.log(e);
		// console.log(String.fromCharCode(e.charCode));
		// console.log(e.key);
		switch(String.fromCharCode(e.charCode)){
				case "t":
				tog("transaction");
				break;
				case "c":
				tog("comment");
				break;
				case "y":
				tog("duringYear");
				break;
				case "i":
				tog("Innk");
				break;
				case "m":
				tog("Mo");
				break;
		}
});


}
