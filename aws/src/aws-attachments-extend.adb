-----------------------------------------------------------------------
--  aws-attachments-extend -- ASF extensions for AWS attachments
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body AWS.Attachments.Extend is

   --  ------------------------------
   --  Get the length of the data content.
   --  ------------------------------
   function Get_Length (E : in Element) return Natural is
   begin
      case E.Kind is
         when Data =>
            return E.Data.Length;

         when others =>
            return 0;
      end case;
   end Get_Length;

   --  ------------------------------
   --  Get the name of the attachment.
   --  ------------------------------
   function Get_Name (E : in Element) return String is
   begin
      case E.Kind is
         when Data =>
            return To_String (E.Data.Content_Id);

         when others =>
            return "";
      end case;
   end Get_Name;

end AWS.Attachments.Extend;
