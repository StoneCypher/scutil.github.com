<?php





  abstract class ScPhpTest_Result {





      public function ResultType() {

          if ($this->isFail()) { return 'fail'; }
          if ($this->isWarn()) { return 'warn'; }
          if ($this->isPass()) { return 'pass'; }
    
          return 'Internal Error: type did not match Fail, Warn or Pass (impossible!)';

      }
  
  
  
  
  
      abstract public function isPass();
      abstract public function isWarn();
      abstract public function isFail();





  };





?>