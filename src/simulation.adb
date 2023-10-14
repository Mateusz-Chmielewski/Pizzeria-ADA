-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;


procedure Simulation is
   Number_Of_Products: constant Integer := 5;
   Number_Of_Assemblies: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;
   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   Product_Name: constant array (Product_Type) of String(1 .. 8)
     := ("Dough   ", "Cheese  ", "Salami  ", "Mushroom", "Ananas  ");
   Assembly_Name: constant array (Assembly_Type) of String(1 .. 12)
     := ("Hawai Pizza ", "Salami Pizza", "Capricciosa ");
   package Random_Assembly is new
     Ada.Numerics.Discrete_Random(Assembly_Type);
   type My_Str is new String(1 ..256);


   task type Producer is
      entry Start(Product: in Product_Type; Production_Time: in Integer);
   end Producer;

   task type Consumer is
      entry Start(Consumer_Number: in Consumer_Type;
                  Consumption_Time: in Integer);
   end Consumer;

   task type Buffer is
      entry Take(Product: in Product_Type; Number: in Integer);
      entry Deliver(Assembly: in Assembly_Type; Number: out Integer);
   end Buffer;


   Producers: array ( 1 .. Number_Of_Products ) of Producer;
   Customers: array ( 1 .. Number_Of_Consumers ) of Consumer;
   Store_Buffer: Buffer;


   task body Producer is
      subtype Production_Time_Range is Integer range 3 .. 6;
      package Random_Production is new
        Ada.Numerics.Discrete_Random(Production_Time_Range);
      Random_Production_Generator: Random_Production.Generator;
      Product_Type_Number: Integer;
      Product_Number: Integer;
      Production: Integer;
   begin

      accept Start(Product: in Product_Type; Production_Time: in Integer) do
         Random_Production.Reset(Random_Production_Generator);
         Product_Number := 1;
         Product_Type_Number := Product;
         Production := Production_Time;
      end Start;

      Put_Line("Started producer of " & Product_Name(Product_Type_Number));
      loop
         delay Duration(Random_Production.Random(Random_Production_Generator));
         Put_Line("Produced product " & Product_Name(Product_Type_Number)
                  & " number "  & Integer'Image(Product_Number));

         --  Store_Buffer.Take(Product_Type_Number, Product_Number);
         Product_Number := Product_Number + 1;
      end loop;

   end Producer;


   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new
        Ada.Numerics.Discrete_Random(Consumption_Time_Range);
      Random_Consumption_Generator: Random_Consumption.Generator;
      Random_Assembly_Generator: Random_Assembly.Generator;
      Consumer_Nb: Consumer_Type;
      Assembly_Number: Integer;
      Consumption: Integer;
      Assembly_Type: Integer;
      Consumer_Name: constant array (1 .. Number_Of_Consumers) of String(1 .. 9)
        := ("Agnieszka", "Kunegunda");

   begin


      accept Start(Consumer_Number: in Consumer_Type;
		  Consumption_Time: in Integer) do
         Random_Consumption.Reset(Random_Consumption_Generator);
         Random_Assembly.Reset(Random_Assembly_Generator);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;

      Put_Line("Started consumer " & Consumer_Name(Consumer_Nb));
      loop
         delay Duration(Random_Consumption.Random(Random_Consumption_Generator));
         Assembly_Type := Random_Assembly.Random(Random_Assembly_Generator);

         select
            delay 10.0;
            Put_Line("Time out");
         then abort
            loop
               Store_Buffer.Deliver(Assembly_Type, Assembly_Number);
               if Assembly_Number > -1 then
                  Put_Line(Consumer_Name(Consumer_Nb) & ": taken assembly " &
                             Assembly_Name(Assembly_Type) & " number " &
                             Integer'Image(Assembly_Number));
                  exit;
               end if;
               delay 1.0;
            end loop;
         end select;
      end loop;
   end Consumer;


   task body Buffer is
      Storage_Capacity: constant Integer := 30;
      type Storage_type is array (Product_Type) of Integer;
      Storage: Storage_type
        := (5, 5, 5, 5, 5);

      Assembly_Content: array(Assembly_Type, Product_Type) of Integer
        := ((1, 1, 0, 0, 2),
            (1, 1, 0, 1, 0),
            (1, 2, 2, 2, 0));

      Max_Assembly_Content: array(Product_Type) of Integer;
      Assembly_Number: array(Assembly_Type) of Integer
        := (1, 1, 1);

      In_Storage: Integer := 0;

      procedure Setup_Variables is
      begin
         for W in Product_Type loop
            Max_Assembly_Content(W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content(Z, W) > Max_Assembly_Content(W) then
                  Max_Assembly_Content(W) := Assembly_Content(Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept(Product: Product_Type) return Boolean is
         Free: Integer;		--  free room in the storage
         -- how many products are for production of arbitrary assembly
         Lacking: array(Product_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary assembly
         Lacking_room: Integer;
         MP: Boolean;			--  can accept
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         end if;
         -- There is free room in the storage
         Free := Storage_Capacity - In_Storage;
         MP := True;

         for W in Product_Type loop
            if Storage(W) < Max_Assembly_Content(W) then
               MP := False;
            end if;
         end loop;

         if MP then
            return True;		--  storage has products for arbitrary
            --  assembly
         end if;

         if Integer'Max(0, Max_Assembly_Content(Product) - Storage(Product)) > 0 then
            -- exactly this product lacks
            return True;
         end if;

         Lacking_room := 1;			--  insert current product
         for W in Product_Type loop
            Lacking(W) := Integer'Max(0, Max_Assembly_Content(W) - Storage(W));
            Lacking_room := Lacking_room + Lacking(W);
         end loop;

         if Free >= Lacking_room then
            -- there is enough room in storage for arbitrary assembly
            return True;
         else
            -- no room for this product
            return False;
         end if;
      end Can_Accept;

      function Can_Deliver(Assembly: Assembly_Type) return Boolean is
      begin
         for W in Product_Type loop
            if Storage(W) < Assembly_Content(Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         for W in Product_Type loop
            Put_Line("Storage contents: " & Integer'Image(Storage(W)) & " "
                     & Product_Name(W));
         end loop;
      end Storage_Contents;

   begin
      Put_Line("Buffer started");
      Setup_Variables;
      loop
         select
            accept Take(Product: in Product_Type; Number: in Integer) do
               if Can_Accept(Product) then
                  Put_Line("Accepted product " & Product_Name(Product) & " number " &
                             Integer'Image(Number));
                  Storage(Product) := Storage(Product) + 1;
                  In_Storage := In_Storage + 1;
               else
                  Put_Line("Rejected product " & Product_Name(Product) & " number " &
                             Integer'Image(Number));
               end if;
            end Take;
         or
            accept Deliver(Assembly: in Assembly_Type; Number: out Integer) do
               if Can_Deliver(Assembly) then
                  Put_Line("Delivered assembly " & Assembly_Name(Assembly) & " number " &
                             Integer'Image(Assembly_Number(Assembly)));
                  for W in Product_Type loop
                     Storage(W) := Storage(W) - Assembly_Content(Assembly, W);
                     In_Storage := In_Storage - Assembly_Content(Assembly, W);
                  end loop;
                  Number := Assembly_Number(Assembly);
                  Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
               else
                  Put_Line("Lacking products for assembly " & Assembly_Name(Assembly));
                  Number := -1;
               end if;
               Storage_Contents;
            end Deliver;


         end select;
      end loop;

   end Buffer;

begin
   --  for I in 1 .. Number_Of_Products loop
   --     Producers(I).Start(I, 10);
   --  end loop;
   for J in 1 .. Number_Of_Consumers loop
      Customers(J).Start(J,12);
   end loop;
end Simulation;

