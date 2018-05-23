$(document).on('click', '.selectable div table tbody tr', function(e){
	var el = $(this);
	if (!e.shiftKey){
		$(this).siblings().removeClass("rowsSelected");
	}
	$(this).addClass("rowsSelected", this.clicked);
	el.trigger("change");
});	
var selectRowBinding = new Shiny.InputBinding();
$.extend(selectRowBinding, {
	find: function(scope) {
		return $(scope).find(".selectable");
	},
	getValue: function(el){
    tbl = $(el).find("table");
    var out = [];
    $rows = $(tbl).children().children('.rowsSelected');
    if($rows.length == 0) return -1;

//    uncomment to return row numbers instead 
//    (would be wrong if table is sorted or filtered)

//    $rows.each(function(row, v){
//      out[row] = $(v).index() + 1;
//    });
//  return out;    

    $rows.each(function(row,v) {
      $(this).find("td").each(function(cell,v) {
        if (typeof out[row] === 'undefined') out[row] = [];
        out[row][cell] = $(this).text();
      });
    });
    return out;
	},
	setValue: function(el, value) {
	},
	subscribe: function(el, callback) {
		$(el).on("change.selectRowBinding", function(e) {
			callback();
		});
	},
	unsubscribe: function(el) {
	  $(el).off(".selectRowBinding");
	}
});
Shiny.inputBindings.register(selectRowBinding);