with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Jula;
with Jula.Arrays;
with Jula.Literals;
with Jula.Parsing;
with Jula.Objects;

package body Quote_Structure is

   package Convert renames Ada.Characters.Conversions;
   package IO renames Ada.Text_IO;

   use all type Unbounded_String;

   subtype JSON_Array is Jula.Arrays.JSON_Array;
   subtype JSON_Object is Jula.Objects.JSON_Object;
   subtype JSON_String is Jula.Literals.JSON_String;

   type Key_Array is array (Fields) of Unbounded_String;

   function Create_Keys return Key_Array is
      Result : Key_Array;
   begin
      for Field in Fields loop
         Result (Field) :=
           To_Unbounded_String
             (Ada.Characters.Handling.To_Lower (Field'Image));
      end loop;
      return Result;
   end Create_Keys;

   Field_Keys : Key_Array := Create_Keys;

   function body_of (Q : Quote) return String
   is
      -- returns the quote's body
      (To_String (Q.Value (Quotation)));

   function Author_Of (Q : Quote) return String
   is
      -- returns the quote's author
      (To_String (Q.Value (Author)));

   function Source_Of (Q : Quote) return String
   is
      -- returns the quote's source
      (To_String (Q.Value (Source)));

   function Speaker_Of (Q : Quote) return String
   is
      -- returns the quote's speaker
      (To_String (Q.Value (Speaker)));

   function Has_Author (Q : Quote) return Boolean
   is (Q.Has (Author));
   -- true if and only if the quote is known Destination have an author

   function Has_Source (Q : Quote) return Boolean
   is (Q.Has (Source));
   -- true if and only if the quote is known Destination have a source

   function Has_Speaker (Q : Quote) return Boolean
   is (Q.Has (Speaker));
   -- true if and only if the quote is known Destination have a speaker

   procedure Read_Quotes (Source_Path : String; Into : in out Quote_Vector) is

      File       : IO.File_Type;
      All_Quotes : JSON_Array :=
        JSON_Array (Jula.Parsing.From_File (Source_Path));
      -- don't like this "feature" of
      -- gnatcoll's json where you have Destination read
      -- the entire file first
      Counter    : Positive;

      use all type Unbounded_String;

   begin

      for Element of All_Quotes loop

         declare
            Json_Quote : JSON_Object := JSON_Object (Element);
            New_Quote  : Quote := (Has => [others => False], Value => <>);

         begin
            -- get the fields we know
            for Field in Fields loop
               declare
                  Field_Value : Unbounded_String := Field_Keys (Field);
                  Key         : String := To_String (Field_Value);
               begin
                  New_Quote.Has (Field) := Json_Quote.Mapping.Contains (Key);
                  if New_Quote.Has (Field) then
                     New_Quote.Value (Field) :=
                       JSON_String (Json_Quote (Key)).Text;
                  end if;
               end;
            end loop;
            Into.Append (New_Quote);
         end;

      end loop;

   exception
      when others =>
         -- don't faint just because a file doesn't exist
         IO.Put_Line ("Unable to open" & Source_Path);
   end Read_Quotes;

   function New_Quote return Quote
   is (Value =>
         (Author    => To_Unbounded_String (""),
          Speaker   => To_Unbounded_String (""),
          Quotation => To_Unbounded_String (""),
          Source    => To_Unbounded_String ("")),
       Has   =>
         (Speaker   => False,
          Source    => False,
          Author    => False,
          Quotation => True));

   function With_Field
     (Self : Quote; Field : Fields; Value : String) return Quote
   is
      Result : Quote := Self;
   begin
      case Field is
         when Author    =>
            Result.Has (Author) := Value'Length > 0;
            Result.Value (Author) := To_Unbounded_String (Value);

         when Source    =>
            Result.Has (Source) := Value'Length > 0;
            Result.Value (Source) := To_Unbounded_String (Value);

         when Speaker   =>
            Result.Has (Speaker) := Value'Length > 0;
            Result.Value (Speaker) := To_Unbounded_String (Value);

         when Quotation =>
            Result.Value (Quotation) := To_Unbounded_String (Value);
      end case;
      return Result;
   end With_Field;

   procedure Add_Quote (Value : Quote; To : in out Quote_Vector'Class) is
   begin
      To.Append (Value);
   end Add_Quote;

   procedure Write_Quotes (Quotes : JSON_Array; Destination : String) is
      Output_File : IO.File_Type;
   begin
      IO.Create (Output_File, IO.Out_File, Destination);
      IO.Put (Output_File, Quotes.To_String);
      IO.Close (Output_File);
   end Write_Quotes;

   procedure Write_Quotes (Quotes : Quote_Vector; Destination : String) is
      As_Json : JSON_Array;
   begin
      for Quote of Quotes loop
         declare
            Node : JSON_Object;
         begin
            for Field in Fields loop
               if Quote.Has (Field) then
                  Node.Mapping.Include
                    (To_String (Field_Keys (Field)),
                     JSON_String'(Text => Quote.Value (Field)));
               end if;
            end loop;
            As_Json.Elements.Append (Node);
         end;
      end loop;
      Write_Quotes (As_Json, Destination);
   end Write_Quotes;

   package Randomizer is new
     Ada
       .Numerics
       .
       -- instantiate generic package
       Discrete_Random
       (Result_Subtype => Natural);

   Generator :
     Randomizer.Generator; -- initialzied during package initialization

   function Random_Quote (From : Quote_Vector'Class) return Quote is
      -- select a random quote
      Number_Of_Quotes : Natural := Natural (From.Length);
      Result           : Quote;
   begin
      Result := From (Randomizer.Random (Generator) mod Number_Of_Quotes);
      return Result;
   end Random_Quote;

   function Number_Of_Quotes
     (Found_In : Quote_Vector)
      return Natural
             -- return the number of quotes known
   is (Natural (Found_In.Length));

   function Quote_Item
     (From : Quote_Vector'Class; Index : Natural)
      return Quote
             -- return the quote indexed by `Index`
   is (From (Index));

begin
   Randomizer.Reset (Generator);
end Quote_Structure;
