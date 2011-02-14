var websocket;
var output;  
var c;
var logdiv;

window.onload = 
    function() {
	console.log('<p>started');
    }

    function loadScript(File) {
	var s = document.createElement('script');
	s.type = 'text/javascript';
	s.src = File;
	s.onload = function(){send('loaded')};
	document.body.appendChild(s);
  }

  function onClose(evt) {
      console.log('<p><b>closed</b>');
      document.body.style.backgroundColor='#aabbcc';
  }  
  
  function onMessage(evt) {
      try
      {
	  eval(evt.data);
      }
      catch(e)
      {
	  alert("oops:"+evt.data);
      }
  }
  
  function onError(evt) { 
      document.body.style.backgroundColor='orange';
  }  
  
  function send(msg) {
      websocket.send(msg);
  }
  
  function log(x) {
      logdiv.innerHTML += x;
  }
  
  function start_session(wsUri){
      console.log("<br>try to connect");
      document.body.style.backgroundColor='#ccaabb';
      websocket           = new WebSocket(wsUri); 
      websocket.onopen    = onOpen;
      websocket.onclose   = onClose;
      websocket.onmessage = onMessage; 
      websocket.onerror   = onError;
      return(false);
    }  
    
    function onOpen(evt) { 
	console.log("connected");
    }

