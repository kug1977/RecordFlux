with RFLX.Types;

package SPARK.Assertions
  with SPARK_Mode
is

   procedure Assert (Condition : Boolean;
                     Message   : String);

   function Assert (Condition : Boolean;
                    Message   : String) return Boolean;

   procedure Assert (Actual    : String;
                     Expected  : String;
                     Message   : String);

   procedure Assert (Actual   : RFLX.Types.Bytes;
                     Expected : RFLX.Types.Bytes;
                     Message  : String);

end SPARK.Assertions;
