function del(){ 
    if(!confirm("Confirmed?")){ 
	    window.event.returnValue = false; 
    } 
}
runproteinpaint({
    host:'https://proteinpaint.stjude.org',
    holder:document.getElementById('aaa'),
    genome:'hg19',
    gene:'TP53',
    dataset:'clinvar'
})
