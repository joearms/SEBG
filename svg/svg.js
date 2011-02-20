/*
 * This file defines a single function SVG to use the module
 * var x = SVG();
 * x.mk_canvas({width:200, id:1, ht:100, color:blue});
 * x.mk_circle({parent:1, cx:10, cy:20, r:15, color:"green"});
 * ...
 */

function SVG(pageId){
    var C = {};
    var V = new Array();
    C.svg_ns = 'http://www.w3.org/2000/svg';
    C.xhtml_ns = 'http://www.w3.org/1999/xhtml';
    C.xlinkns = 'http://www.w3.org/1999/xlink';
    // drag variables
    C.xstart = 0;
    C.ystart = 0;
    C.dragging = false;
    C.dragobj = null;
    C.canvas = null;
    C.pageId = pageId;
    // alert("PageId"+pageId);
    // mk_canvas is a top-level call
    
    C.mk_canvas = function(o)
    {
	var x = document.createElementNS(this.svg_ns, 'svg');
	x.setAttribute("width", D(o.width,700));
	x.setAttribute("height", D(o.ht, 200));
	x.setAttribute("style","background-color:" + D(o.color,"red"));
	x.setAttribute("id", o.id);
	x.addEventListener("mousemove", drag, false);
	x.addEventListener("mousedown", mouse_down, false);
	x.addEventListener("mouseup", mouse_up, false);
	C.canvas = x;
	V[o.id]=x;
	return x;
    };
    
    C.mk_group = function(o)
    {
	var obj = document.createElementNS(this.svg_ns, 'g');
	var x = D(o.x,10);
	var y = D(o.y,10);
	var t = "translate("+x+","+y+")";
	obj.setAttribute('transform', t);
	obj.setAttribute('x',x);
	obj.setAttribute('y',y);
	obj.setAttribute('draggable','true');
	obj.setAttribute("id", o.id);
	log("draggable "+o.id);
	setId(o.id, obj);
	V[o.parent].appendChild(obj);
	return obj;
    };

    function D(x, y){
	return x == undefined ? y : x;
    };
    
    // regular DOM objects into the SVG
    C.mk_button = function(o)
    {
	var fo = document.createElementNS(this.svg_ns,"foreignObject");
	fo.setAttribute("x", D(o.x, 10));
	fo.setAttribute("y", D(o.y, 100));
	fo.setAttribute("width", 200);
	fo.setAttribute("height", 50);
	V[o.parent].appendChild(fo);
	setId(o.id, fo);
	var n = document.createElement('input');
	n.setAttribute('type','button');
	n.setAttribute('value',D(o.txt, 'click me'));
	n.setAttribute('name','joe');
	fo.appendChild(n);
	return fo;
    };		
    	
    C.mk_rect = function (o)
    {
	var obj = document.createElementNS(this.svg_ns, 'rect');
	obj.setAttribute('x', o.x);
	obj.setAttribute('y', o.y);
	obj.setAttribute('width', o.width);
	obj.setAttribute('height', o.ht);
	obj.setAttribute('fill', o.color);
	obj.setAttribute("rx", D(o.rx, 5));
	obj.setAttribute("ry", D(o.ry, 5));
	obj.setAttribute("id", o.id);
	setId(o.id, obj);
	V[o.parent].appendChild(obj);
	return obj;
    };

    function setId(id, obj){
	log("V["+id+"]="+obj);
	V[id] = obj;
    };
    
    C.mk_circle = function(o)
    {
	var obj = document.createElementNS(this.svg_ns, 'circle');
	obj.setAttribute('cx', o.x);
	obj.setAttribute('cy', o.y);
	obj.setAttribute('r', o.r);
	obj.setAttribute('fill', D(o.color,"red"));
	obj.setAttribute("id", o.id);
	setId(o.id, obj);
	V[o.parent].appendChild(obj);
	return obj;
    };
    
    C.mk_text = function(o) 
    {
	var text = document.createElementNS(this.svg_ns, "text");
	text.setAttribute("fill", D(o.fill,"green"));
	var size = D(o.size, 1);
	text.setAttribute("font-size", size+"em");
	var font = D(o.font,"Arial");
	text.setAttribute("font-family", font);
	text.setAttribute("x", D(o.x,10));
	text.setAttribute("y", D(o.y,10));
	text.setAttribute("text-anchor", D(o.anchor,"start"));
	var str = D(o.str,"** missing str in text **");
	var data = document.createTextNode(o.str);
	text.setAttribute("id", o.id);
	setId(o.id, text);
	text.appendChild(data);
	V[o.parent].appendChild(text);
	return text;
    };
    
    C.mk_image = function(o)
    {
	var img = document.createElementNS(this.svg_ns, "image");  
	img.setAttribute("x", D(o.x,10));
	img.setAttribute("y", D(o.y,10));
	img.setAttribute("width", D(o.width, 20));
	img.setAttribute("height", D(o.ht, 20));
	img.setAttributeNS(this.xlinkns, "href", o.img);
	img.setAttribute("id", o.id);
	setId(o.id, img);
	V[o.parent].appendChild(img);
	return img;
    };
    
    C.mk_line = function(o)
    {
	var obj= document.createElementNS(this.svg_ns,"line");
	obj.setAttribute("x1", o.x1);
	obj.setAttribute("y1", o.y1);
	obj.setAttribute("x2", o.x2);
	obj.setAttribute("y2", o.y2);
	obj.setAttribute("stroke","black");
	var width = D(o.width,2);
	obj.setAttribute("stroke-width",width+"px");
	obj.setAttribute("fill",D(o.fill,"black"));
	obj.setAttribute("marker-end","url(./svg2.#myMarker)");
	obj.setAttribute("id", o.id);
	V[o.id] = obj;
	V[o.parent].appendChild(obj);
	return obj;
    };
    
    C.mk_ellipse = function(o)
    {
	var obj = document.createElementNS(this.svg_ns, "ellipse");
	obj.setAttribute("cx", o.cx);
	obj.setAttribute("cy", o.cy);
	obj.setAttribute("rx", o.rx);
	obj.setAttribute("ry", o.ry);
	obj.setAttribute("fill", D(o.fill,"black"));
	obj.setAttribute("id", o.id);
	V[o.id] = obj;
	V[o.parent].appendChild(obj);
	return obj;
    };

    C.configure = function(o)
    {
	log("config "+V[o.id]+" key="+o.key+" val="+o.val);
	V[o.id].setAttribute(o.key, o.val);
	log("confok");
    };

    C.render = function(objs){
    	var val;
	for(var i = 0; i < objs.length; i++){
	    var o = objs[i];
	    switch(o.cmd){
	    case "mk_canvas":  val = C.mk_canvas(o);  break;
	    case "mk_rect":    val = C.mk_rect(o);    break;
	    case "mk_circle":  val = C.mk_circle(o);  break;
	    case "mk_text":    val = C.mk_text(o);    break;
	    case "mk_ellipse": val = C.mk_ellipse(o); break;
	    case "mk_line":    val = C.mk_line(o);    break;
	    case "mk_group":   val = C.mk_group(o);   break;
	    case "mk_image":   val = C.mk_image(o);   break;
	    case "mk_button":  val = C.mk_button(o);  break;
	    case "configure":  C.configure(o);        break;
	    default: alert("bad o.cmd ="+o.cmd);
	    };
	    if(o.clickable){
		log("clickable "+o.id + "val="+o.msg);
		val.setAttribute("message", D(o.msg, "void"));
		val.addEventListener("mousedown", 
				     function(evt){was_clicked(evt)},
				     false);
	    };
	};
    };

    function was_clicked(evt)
    {
	obj = evt.target;
	var id  = obj.getAttribute("id");
	var msg  = obj.getAttribute("message");
	var str = "clicked?page=" +
	    "&id=" + escape(id) +
	    "&msg=" + escape(msg);
	svg_event(str); 
    };

    function mouse_down(evt)
    {
	// log("mouse down");
	var t = evt.target.parentNode;
	// log("target="+t);
	this.dragging  = t.getAttribute("draggable");
	if(this.dragging){
	    this.dragobj = t;
	    var x = evt.clientX + window.scrollX;
	    var y = evt.clientY + window.scrollY;
	    var cxstart = parseInt(t.getAttribute("x"));
	    var cystart = parseInt(t.getAttribute("y"));
	    this.xstart = x - cxstart;
	    this.ystart = y - cystart;
	}
    };
    
    function mouse_up(evt)
    {
	if(this.dragging){
	    this.dragging = false;
	    // log("dragging=false");
	    var obj = this.dragobj;
	    // alert(obj);
	    var x = obj.getAttribute("x");
	    var y = obj.getAttribute("y");
	    // log("dragend = " + obj.id + " x=" + x + " y= "+y);
	    svg_event({pageId:C.pageId, type:"drag", id:obj.id, x:x, y:y});
	};
    };
    
    function drag(evt) 
    {
	if (this.dragging) 
	    {
		var t = evt.target;
		var x = evt.clientX + window.scrollX;
		var y = evt.clientY + window.scrollY;
		// Move drag element by the same amount the cursor has moved.
		var x1 = (x-this.xstart);
		var y1 = (y-this.ystart);
		this.dragobj.setAttribute("x", x1);
		this.dragobj.setAttribute("y", y1);
		var t = "translate("+x1+","+y1+")";
		this.dragobj.setAttribute("transform", t);
	    }
    };

    return C;
}
