<?php

function input_filename($$create_path = false) {
  $$path = get_session_path();
  if ($$create_path) system("mkdir -p $$path");
  return "$$path/input";
}


$$inputFile=input_filename(true);
   
if (isset($$_FILES['userfile']) && $$_FILES['userfile']['name']) {
   move_uploaded_file($$_FILES['userfile']['tmp_name'], $$inputFile);
} else if (get_field('exampleList','none') !== 'none' ) {
   copy(get_field('exampleList','none'),$$inputFile);
} else if (get_field('textInput','empty') !== 'empty' ) {
   file_put_contents($$inputFile, get_field('textInput','empty'));
} else { $$inputFile = '$defaultInput$'; };
   
$$problem = file_get_contents($$inputFile);   
?>


<?php print_r($$_POST); ?>

<div class='webif'>
  <form name="inputForm" enctype="multipart/form-data" action="#output" method="POST"> 
    <span class="head">Input</span>
    &nbsp;<i>(in <a href="http://www.lri.fr/%7Emarche/tpdb/format.html" onclick="window.open(this.href);return false;">trs</a> format)</i>
    <br>
    
    <div class='input-select'>
      <select class='input-list' name='exampleList' onchange="this.form.submit();">
	<option value='none'>select example …</option>
	$for(examples)$
          <option value='$filepath$' <?php if (field_set_as('exampleList','$filepath$')) {print 'selected';}; ?>>$name$</option>
	$endfor$
      </select>
      or upload file
      <input label="browse" name="userfile" type="file" size="20" onchange="this.form.submit()" title="Upload a file in trs format."/>
      <textarea id="textInput" style="width:85%" rows=7 name="textInput" spellcheck="false"><?=$$problem?></textarea>
    </div>
  </form>
</div>
