<?php





  final class ScPhpTest_Result_Warn extends ScPhpTest_Result {





      public function isPass() { return false; }
      public function isWarn() { return true;  }
      public function isFail() { return false; }





      public function __construct($cReason = 'No reason given') {
        $reason = $cReason;
      }





      public $reason = false;





  };





?>