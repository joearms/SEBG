svg_ns = 'http://www.w3.org/2000/svg';
    
document.onkeydown = function(e){update(e);};

    
var globstr = "Hello World";
var text;
var svg;

D = function(x, y){
    return x == undefined ? y : x;
};
    
update = function(e) {
    cc = e.keyCode;
    // can browse this in the log window
    console.log("pressed",e);
    console.log("pressed="+e.keyCode+" shift="+e.shiftKey);
    globstr += String.fromCharCode(cc);
    change_text(globstr);
    return false;
}


mk_canvas = function(o) {
    var x = document.createElementNS(svg_ns, 'svg');
    x.setAttribute("width", D(o.width,700));
    x.setAttribute("height", D(o.height, 200));
    x.setAttribute("style","background-color:" + D(o.color,"red"));
    x.setAttribute("id", o.id);
    x.addEventListener("mousemove", drag, false);

    // x.addEventListener("mousedown", mouse_down, false);
    // x.addEventListener("mouseup", mouse_up, false);
    return x;
};
    
mk_rect = function(o) {
    var obj = document.createElementNS(svg_ns, 'rect');
    obj.setAttribute('x', D(o.x, 10));
    obj.setAttribute('y', D(o.y,20));
    obj.setAttribute('width', D(o.width,120));
    obj.setAttribute('height', D(o.ht, 40));
    obj.setAttribute('fill', D(o.color,'#aabb11'));
    obj.setAttribute('stroke','black');
    obj.setAttribute('stroke-width',3);
    obj.setAttribute("rx", D(o.rx, 5));
    obj.setAttribute("ry", D(o.ry, 5));
    obj.addEventListener("click", buttonClicked, false);
    obj.addEventListener("mouseover", buttonOver, false);
    obj.addEventListener("mouseout", buttonOut, false);
    obj.addEventListener("onkeydown", key1, false);
    return obj;
};

key1 = function(evt){
    console.log("key1");
}

buttonClicked = function(evt) {
    var x =evt.target;
    x.setAttribute("fill", "orange");
};

buttonOver = function(evt) {
    var x =evt.target;
    x.setAttribute("fill", "red");
};

buttonOut = function(evt) {
    var x =evt.target;
    x.setAttribute("fill", "#aabb11");
};

keypress = function(evt) {
    // var x = evt.target;
    console.log("pressed");
}

mk_text = function(o) {
    var text = document.createElementNS(svg_ns, "text");
    text.setAttribute("fill", D(o.fill,"green"));
    var size = D(o.size, 1);
    text.setAttribute("font-size", size+"px");
    var font = D(o.font,"Courier");
    text.setAttribute("font-family", font);
    text.setAttribute("x", D(o.x,10));
    text.setAttribute("y", D(o.y,10));
    text.setAttribute("text-anchor", D(o.anchor,"start"));
    var str = D(o.str,"** missing str in text **");
    var data = document.createTextNode(globstr);
    text.setAttribute("id", o.id);
    text.appendChild(data);
    return text;
};



change_text = function(x) {
    text.textContent=x;
}


window.onload = function() {
    svg = mk_canvas({width:800, height:200, color:'#ffeecc',id:1}),
    c = document.body.appendChild(svg);
    c.appendChild(mk_rect({}));
    text = mk_text({x:250,y:50, size:24, str:"Hello Joe"})
    c.appendChild(text);
}


function drag(evt) 
{
    var t = evt.target;
    // console.log("drag="+t);
}
