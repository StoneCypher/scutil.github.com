<?php

  // $Revision$

  // can't use class wrapped version enforcement in case the class breaks from too-old php before it gets there, lol
  // always use the class method version instead plskthx

  function EnforceMinimumPhpVersion($ver, $message)

      if (!(version_compare(phpversion(), $ver) === 1)) {

          die (
          );

      }

  }

  EnforceMinimumPhpVersion('5.1.6', '<html><head><title>Needs newer PHP</title></head><body><p>Requires a minimum PHP version of <tt>5.1.6</tt> .</p></body></html>');

?>
