// code cut-and-paste from
// http://dev.day.com/content/ddc/blog/2010/06.html

// console.log('injecting code');

    function getContent() {
	// console.log('getting selection');
	var selection = window.getSelection( );
	var markup = serializeSelection( selection );  
	// var finalMarkup = formatPage( markup );
	// return finalMarkup;
	return markup;
    }

    function serializeSelection( selection ) {
        var xmlFragment = "";
        try {
            var n = 0, ranges = selection.rangeCount;
	    // console.log('ranges='+ranges); 
            while ( n != ranges ) {
                var range = selection.getRangeAt( n++ );
		// console.log('here',range);
                var content = range.cloneContents( );
                var serializer = new XMLSerializer( );
                xmlFragment += serializer.serializeToString( content );
            }
        }
        catch( msg ) { }
	return xmlFragment;
    }

    content = getContent();

    // console.log('content',content);


var pageInfo = {
    "title": document.title,
    "url": window.location.href,
    "text": window.getSelection().toString(),
    "html": content
};

    // console.log('sending page',pageInfo)
    // Send the information back to the extension

chrome.extension.sendRequest(pageInfo);

   

