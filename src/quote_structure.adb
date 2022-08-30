with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Numerics.Discrete_Random;
with Gnatcoll.Json; use Gnatcoll.Json;

package body Quote_Structure is

   type Key_Array is array(Fields) of Unbounded_String;

   function Create_Keys return Key_Array is
      Result: Key_Array;
   begin
      for Field in Fields loop
         Result(Field)
            := To_Unbounded_String
               (Ada.Characters.Handling.To_Lower(Field'Image));
      end loop;
      return Result;
   end Create_Keys;

   Field_Keys : Key_Array := Create_Keys;

   function To_String(Source: Unbounded_String) return String
                      renames Ada.Strings.Unbounded.To_String;
   -- convenience renaming

   function body_of(Q: Quote) return String is
   -- returns the quote's body
      (To_String(Q.Value(Quotation)));

   function Author_Of(Q: Quote) return String is
   -- returns the quote's author
      (To_String(Q.Value(Author)));

   function Source_Of(Q: Quote) return String is
   -- returns the quote's source
      (To_String(Q.Value(Source)));

   function Speaker_Of(Q: Quote) return String is
   -- returns the quote's speaker
      (To_String(Q.Value(Speaker)));

   function Has_Author(Q: Quote) return Boolean is (Q.Has(Author));
   -- true if and only if the quote is known to have an author

   function Has_Source(Q: Quote) return Boolean is (Q.Has(Source));
   -- true if and only if the quote is known to have a source

   function Has_Speaker(Q: Quote) return Boolean is (Q.Has(Speaker));
   -- true if and only if the quote is known to have a speaker

   package TIO renames Ada.Text_IO;
   -- convenience renaming

   procedure Read_Quotes(From: String; Into: in out Quote_Vector) is

      File: TIO.File_Type;
      All_Quotes: Json_Array;
      All_Lines : Unbounded_String; -- read from the file,
                                    -- don't like this "feature" of
                                    -- gnatcoll's json where you have to read
                                    -- the entire file first
      Counter   : Positive;
      Parsing_Result: Read_Result;

   begin

      -- read the quotes from the file into All_Lines
      Tio.Open(File, TIO.In_File, From);
      while not TIO.End_Of_File(File) loop
         declare Line: String := Tio.Get_Line(File);
         begin
            Append(All_Lines, Line);
         end;
      end loop;
      Tio.Close(File);

      -- parse as json
      Parsing_Result := Read(All_Lines);

      -- only continue if the parsing was successful
      if Parsing_Result.Success then

         All_Quotes := Get(Parsing_Result.Value);

         -- if I read the documentation of gnatcoll's json correctly,
         -- we cannot do a for loop over All_Quotes, so we go this way instead
         Counter := Array_First(All_Quotes);
         while Array_Has_Element(All_Quotes, Counter) loop

            declare
               Json_Quote: Json_Value := Array_Element(All_Quotes, Counter);
               New_Quote : Quote;

            begin
               -- get the fields we know
               for Field in Fields loop
                  New_Quote.Has(Field)
                     := Json_Quote.Has_Field(To_String(Field_Keys(Field)));
                  if New_Quote.Has(Field) then
                     New_Quote.Value(Field)
                        := Json_Quote.Get(To_String(Field_Keys(Field)));
                  end if;
               end loop;
               Into.Append(New_Quote);
            end;

            Counter := Array_Next(All_Quotes, Counter);

         end loop;

      end if;

   exception
      when others => -- don't faint just because a file doesn't exist
         null;
   end Read_Quotes;

   function New_Quote return Quote is
      (
       Value => (
                 Author      => To_Unbounded_String(""),
                 Speaker     => To_Unbounded_String(""),
                 Quotation   => To_Unbounded_String(""),
                 Source      => To_Unbounded_String("")
                ),
       Has   => (
                 Speaker => False,
                 Source  => False,
                 Author  => False,
                 Quotation => True
                )
      );

   function With_Field(Self: Quote; Field: Fields; Value: String)
                       return Quote
   is
      Result: Quote := Self;
   begin
      case Field is
         when Author =>
            Result.Has(Author)   := Value'Length > 0;
            Result.Value(Author) := To_Unbounded_String(Value);
         when Source =>
            Result.Has(Source)   := Value'Length > 0;
            Result.Value(Source) := To_Unbounded_String(Value);
         when Speaker =>
            Result.Has(Speaker)   := Value'Length > 0;
            Result.Value(Speaker) := To_Unbounded_String(Value);
         when Quotation =>
            Result.Value(Quotation) := To_Unbounded_String(Value);
      end case;
      return Result;
   end With_Field;

   procedure Add_Quote(Value: Quote; To: in out Quote_Vector'Class) is
   begin
      To.Append(Value);
   end Add_Quote;

   procedure Write_Quotes(Quotes: JSON_Value; To: String) is
      Output_File: TIO.File_Type;
   begin
      TIO.Create(Output_File, TIO.Out_File, To);
      TIO.Put(Output_File, Write(Quotes, False));
      TIO.Close(Output_File);
   end Write_Quotes;

   procedure Write_Quotes(Quotes: Quote_Vector; To: String) is
      Data: JSON_Array := GNATCOLL.JSON.Empty_Array;
      Output: JSON_Value;
   begin
      for Quote of Quotes loop
         declare Node: JSON_Value := Create_Object;
         begin
            for Field in Fields loop
               if Quote.Has(Field) then
                  Node.Set_Field(To_String(Field_Keys(Field)),
                                 Quote.Value(Field));
               end if;
            end loop;
            Append(Data, Node);
         end;
      end loop;
      Output := Create(Data);
      Write_Quotes(Output, To);
   end Write_Quotes;

   package Randomizer is new Ada.Numerics.Discrete_Random
   -- instantiate generic package
      (Result_Subtype => Natural);

   Generator: Randomizer.Generator; -- initialzied during package initialization

   function Random_Quote(From: Quote_Vector'Class) return Quote is
   -- select a random quote
      Number_Of_Quotes: Natural := Natural(From.Length);
      Result: Quote;
   begin
      Result := From(Randomizer.Random(Generator) mod Number_Of_Quotes);
      return Result;
   end Random_Quote;

   function Number_Of_Quotes(Found_In: Quote_Vector) return Natural is
   -- return the number of quotes known
      ( Natural( Found_In.Length ) );

   function Quote_Item(From: Quote_Vector'Class; Index: Natural) return Quote is
   -- return the quote indexed by `Index`
      ( From(Index) );

begin
   Randomizer.Reset(Generator);
end Quote_Structure;
