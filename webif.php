<?php

function input_filename($$create_path = false) {
  $$path = get_session_path();
  if ($$create_path) system("mkdir -p $$path");
  return "$$path/input";
}


$$inputFile=input_filename(true);
   
if (get_field('exampleList','none') !== 'none' ) {
   copy(get_field('exampleList','none'),$$inputFile);
} else if (isset($$_FILES['userfile']) && $$_FILES['userfile']['name']) {
   move_uploaded_file($$_FILES['userfile']['tmp_name'], $$inputFile);
} else if (get_field('textInput','empty') !== 'empty' ) {
   file_put_contents($$inputFile, get_field('textInput','empty'));
} else {
   copy('$defaultInput$',$$inputFile);   
};
   
$$problem = file_get_contents($$inputFile);

print_r($$_POST);   
   ?>

<script src="javascript/webif.js"></script> 

<div class='webif'>
  <form name="runForm" enctype="multipart/form-data" action="#output" method="POST">
    <div class='input'>  
      <span class="head">Input</span>&nbsp;<i>(in <a href="http://www.lri.fr/%7Emarche/tpdb/format.html" onclick="window.open(this.href);return false;">trs</a> format)</i>
      <div class='input-select'>
	<select class='input-list' name='exampleList' onchange="this.form.submit();">
	  <option value='none'>select example …</option>
	  $for(examples)$
          <option value='$filepath$' <?php if (field_set_as('exampleList','$filepath$')) {print 'selected';}; ?>>$name$</option>
	  $endfor$
	</select>
	or upload file
	<input label="browse" name="userfile" type="file" size="20" onchange="this.form.submit()" title="Upload a file in trs format."/>
      </div>
      <div class='input-box'>
	<textarea name="textInput" spellcheck="false"><?=$$problem?></textarea>
      </div>
    </div>
    
    $partial("templates/strategy-list.html")$
    
  </form>
</div>
