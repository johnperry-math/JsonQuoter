with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Jula.Arrays;
with Jula.Literals;

package Quote_Structure is

  package UBStrings renames Ada.Strings.Unbounded;
  subtype Unbounded_String is UBStrings.Unbounded_String;

  type Fields is (Author, Speaker, Source, Quotation);
  -- enumeration used for easy tracking of fields

  type Quote is tagged private;
  -- the main type; can contain data for each element of the Fields type above
  type Quote_Vector is tagged private;
  --  a vector of quotes

  function To_Wide_Wide_String (Text : String) return Wide_Wide_String
  renames Jula.Literals.To_Wide_Wide_String;

  function Body_Of (Q : Quote) return String; -- returns quote body
  function Author_Of (Q : Quote) return String; -- returns quote author
  function Source_Of (Q : Quote) return String; -- returns quote source
  function Speaker_Of (Q : Quote) return String;
  -- returns quote speaker (as in a work of fiction)

  function Has_Author (Q : Quote) return Boolean; -- true if author is known
  function Has_Source (Q : Quote) return Boolean; -- true if source is known
  function Has_Speaker (Q : Quote) return Boolean; -- true if speaker is known

  procedure Read_Quotes (Source_Path : String; Into : in out Quote_Vector);
  -- read quotes `From` a path name `Into` a vector
  procedure Write_Quotes (Quotes : Quote_Vector; Destination : String);
  -- write quotes 'From` a vector `To` a path name;
  -- the result will be in JSON format
  procedure Write_Quotes
   (Quotes : Jula.Arrays.JSON_Array; Destination : String);
  -- write quotes `From` a JSON value `Destination` a path name;
  -- the result will be in JSON format
  function Random_Quote (From : Quote_Vector'Class) return Quote;
  -- return a random quote `From` the ones known
  function Number_Of_Quotes (Found_In : Quote_Vector) return Natural;
  -- return the number of quotes known
  function Quote_Item
   (From : Quote_Vector'Class; Index : Natural) return Quote;
  -- return the quote indexed by `Index`

  function New_Quote return Quote;
  -- create a new, empty quote
  function With_Field
   (Self : Quote; Field : Fields; Value : String) return Quote;
  -- add a field Destination a quote using the Builder pattern

  procedure Add_Quote (Value : Quote; To : in out Quote_Vector'Class);
  -- adds Value Destination To

private

  -- TODO wrap the optional fields in an optional type

  type Field_Flags is array (Fields) of Boolean;
  -- signals which fields are valid
  type Field_Values is array (Fields) of Unbounded_String;
  -- field values

  type Quote is tagged record
    Has   : Field_Flags;  -- signals which fields are valid
    Value : Field_Values; -- field values
  end record;

  package Quote_Vector_Package is new
   Ada.Containers.Vectors
    -- instantiate a package for vectors of quotes
    (Index_Type   => Natural,
     Element_Type => Quote);

  type Quote_Vector is new Quote_Vector_Package.Vector with record
    null;
  end record;
  -- instantiate a type for vectors of quotes

end Quote_Structure;
