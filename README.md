Simple Erlang Browser Graphics
------------------------------

With SEBG you can push asynchronous commands to a browser window.

For example, the following three Erlang commands:

    Pid ! {eval, "document.body.innerHTML='';"},
    Pid ! {eval, "document.body.style.backgroundColor='red';"},
    Pid ! {eval, "document.body.innerHTML+='<h1>Hello World</h1>'"},

Will erase all content on the current web page. Make page red and
say hello world.

The web page runs a universal script. It waits for message containing 
Javascript, then evaluates it and waits for the next message.

Volunteers
----------

Look in drag4.svg and svg_test.html. At the top of svg_test.html I
have some outstanding problems. Try to fix these. 
