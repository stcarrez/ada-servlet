-----------------------------------------------------------------------
--  aws-attachments-extend -- ASF extensions for AWS attachments
--  Copyright (C) 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package AWS.Attachments.Extend is

   --  Get the length of the data content.
   function Get_Length (E : in Element) return Natural;

   --  Get the name of the attachment.
   function Get_Name (E : in Element) return String;

end AWS.Attachments.Extend;
