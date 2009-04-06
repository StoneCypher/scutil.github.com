<?php

  // $Revision$
  // Since Apr 2, 2009
  // $Date$
  // $HeadURL$
  // $Id$

  // TODO phpDocumentor
  // MIT License, see http://scutil.com/license.html





  require_once('ScUtil.php');





  class ScTagWriter implements ScPhpTestableInterface {





      public static function Make($tag, $conts='', $args=array()) {

          $iArgs = ScUtil::ImplodeSkippingEmpty(' ', $args);

          $close = ($conts == '')? ('/>') : ('>');
          $end   = ($conts == '')? ('')   : ("</$tag>");

          $uArgs = ($iArgs == '')? ('')   : (' ' . implode(' ', $args));

          return "<{$tag}{$uArgs}{$close}{$conts}{$end}";

      }





      public static function LabelledInput($id, $value='', $type='') {

          $utype  = (($type=='')?  '' : "type=\"$type\"";
          $uvalue = (($value=='')? '' : "value=\"$value\"";

          return self::Make('label', self::Make(), array($utype, $uvalue));

      }





  };





?>