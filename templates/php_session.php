<?php
  session_start();

  function get_session_path() {
    return 'session/' . session_id();
  }

  function get_field($$field, $$default) {
    return isset($$_POST[$$field]) ? htmlentities($$_POST[$$field]) : $$default;
  }

  function field_set($$field) {
    return isset($$_POST[$$field]);;
  }
  
  function field_set_as($$field, $$value) {
    return isset($$_POST[$$field]) && $$_POST[$$field] == $$value;
  }

?>
$body$ 

