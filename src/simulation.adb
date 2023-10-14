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

         Store_Buffer.Take(Product_Type_Number, Product_Number);
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
      Assembly_Content_Importance: array(Product_Type) of Float;
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

      function Assembly_Content_Sum(Index: Product_Type) return Float is
         Sum: Float := 0.0;
      begin
         for Z in Assembly_Type loop
            Sum := Sum + Float(Assembly_Content(Z, Index));
         end loop;

         return Sum;
      end Assembly_Content_Sum;

      procedure Calculate_Products_Importance is
         Sum: Float;
      begin
         for W in Product_Type loop
            Sum := Assembly_Content_Sum(W) / Float(Storage_Capacity);
            Assembly_Content_Importance(W) := Sum;
         end loop;
      end Calculate_Products_Importance;

      procedure Remove_Most_Redundant_Product is
         Most_Redundant_Product: Product_Type := 0;
         Most_Product_Redundance: Float := 0.0;
         Product_Redundance: Float := 0.0;
      begin
         for W in Product_Type loop
            Product_Redundance := Float(Storage(W)) / Float(Storage_Capacity);
            if Product_Redundance > Most_Product_Redundance then
               Most_Redundant_Product := W;
               Most_Product_Redundance := Product_Redundance;
            end if;
         end loop;

         if Most_Product_Redundance > Assembly_Content_Importance(Most_Redundant_Product) then
            Storage(Most_Redundant_Product) := Storage(Most_Redundant_Product) - 1;
            In_Storage := In_Storage - 1;

            Put_Line("Removed product " & Product_Name(Most_Redundant_Product) &
                       " number " & Integer'Image(Most_Redundant_Product));
         end if;
      end Remove_Most_Redundant_Product;


      function Can_Accept(Product: Product_Type) return Boolean is
         Storage_Free_Space: Integer;
         -- how many products are for production of arbitrary assembly
         Lacking: array(Product_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary assembly
         Lacking_room: Integer;
         Can_Accept: Boolean;
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         end if;
         Storage_Free_Space := Storage_Capacity - In_Storage;
         Can_Accept := True;

         for W in Product_Type loop
            if Storage(W) < Max_Assembly_Content(W) then
               Can_Accept := False;
            end if;
         end loop;

         if Can_Accept then
            return True;
         end if;

         if Integer'Max(0, Max_Assembly_Content(Product) - Storage(Product)) > 0 then
            return True;
         end if;

         Lacking_room := 1;
         for W in Product_Type loop
            Lacking(W) := Integer'Max(0, Max_Assembly_Content(W) - Storage(W));
            Lacking_room := Lacking_room + Lacking(W);
         end loop;

         if Storage_Free_Space >= Lacking_room then
            return True;
         end if;

         Remove_Most_Redundant_Product;

         if Storage_Free_Space + 1 >= Lacking_room then
            return True;
         else
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
      Calculate_Products_Importance;
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
   for I in 1 .. Number_Of_Products loop
      Producers(I).Start(I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      Customers(J).Start(J,12);
   end loop;
end Simulation;

