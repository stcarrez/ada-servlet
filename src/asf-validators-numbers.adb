-----------------------------------------------------------------------
--  asf-validators-numbers -- ASF Number Validators
--  Copyright (C) 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

package body ASF.Validators.Numbers is

   --  ------------------------------
   --  Create a range validator to check that the value is between the minimum and the maximum.
      --  ------------------------------
   function Create_Range_Validator (Minimum : in Long_Long_Integer;
                                    Maximum : in Long_Long_Integer) return Validator_Access is
      Result : constant Range_Validator_Access := new Range_Validator;
   begin
      Result.Minimum := Minimum;
      Result.Maximum := Maximum;
      return Result.all'Access;
   end Create_Range_Validator;

   --  ------------------------------
   --  Verify that the value is between the validator minimum and maximum
   --  boundaries.
   --  If some error are found, the procedure should create a <b>FacesMessage</b>
   --  describing the problem and add that message to the current faces context.
   --  The procedure can examine the state and modify the component tree.
   --  It must raise the <b>Invalid_Value</b> exception if the value is not valid.
   --  ------------------------------
   procedure Validate (Valid     : in Range_Validator;
                       Context   : in out ASF.Contexts.Faces.Faces_Context'Class;
                       Component : in out ASF.Components.Base.UIComponent'Class;
                       Value     : in EL.Objects.Object) is
   begin
      if EL.Objects.Is_Null (Value) then
         return;
      end if;

      declare
         N : constant Long_Long_Integer := EL.Objects.To_Long_Long_Integer (Value);
      begin
         if N > Valid.Maximum then
            raise Invalid_Value;
         end if;
         if N < Valid.Minimum then
            raise Invalid_Value;
         end if;
      end;
   end Validate;

end ASF.Validators.Numbers;
