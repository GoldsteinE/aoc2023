with ada.strings;
with ada.strings.unbounded;
with ada.text_io;
with ada.text_io.unbounded_io;
with ada.long_integer_text_io;
with ada.command_line;
with ada.containers.vectors;
with ada.containers.ordered_sets;

use ada.strings.unbounded;
use ada.text_io;
use ada.text_io.unbounded_io;
use ada.command_line;

procedure main is
	type galaxy is record
		x : long_integer;
		y : long_integer;
	end record;

	package integer_sets is new ada.containers.ordered_sets ( element_type => integer );
	package lines_vectors is new ada.containers.vectors ( index_type => natural, element_type => unbounded_string );
	package galaxies_vectors is new ada.containers.vectors ( index_type => natural, element_type => galaxy );

	kind : string := argument(1);
	part : string := argument(2);
	increment : long_integer := 1;

	line : unbounded_string;
	height : natural := 0;
	space : lines_vectors.vector;

	line_is_empty : boolean;
	empty_lines : integer_sets.set;
	empty_columns : integer_sets.set;

	gal1 : galaxy;
	gal2 : galaxy;
	galaxies: galaxies_vectors.vector;
	galaxies_count : natural := 0;

	bigx : long_integer;
	bigy : long_integer;
	result : long_integer := 0;
begin
	if part = "2" then
		if kind = "demo" then
			increment := 99;
		else
			increment := 999_999;
		end if;
	end if;

	read_lines : loop
		exit read_lines when end_of_file(current_input);
		line := get_line(current_input);
		lines_vectors.append(space, line);
		height := height + 1;
	end loop read_lines;

	for col_idx in 1 .. length(space(0)) loop
		integer_sets.insert(empty_columns, col_idx);
	end loop;

	for line_idx in 0 .. (height - 1) loop
		line := space(line_idx);
		line_is_empty := true;
		for col_idx in 1 .. length(line) loop
			if element(line, col_idx) = '#' then
				line_is_empty := false;
				integer_sets.exclude(empty_columns, col_idx);
			end if;
		end loop;
		if line_is_empty then
			integer_sets.insert(empty_lines, line_idx);
		end if;
	end loop;

	bigy := 1;
	for line_idx in 0 .. (height - 1) loop
		bigx := 1;
		line := space(line_idx);
		for col_idx in 1 .. length(line) loop
			if integer_sets.contains(empty_columns, col_idx) then
				bigx := bigx + increment;
			end if;
			if element(line, col_idx) = '#' then
				gal1 := (bigx, bigy);
				galaxies_vectors.append(galaxies, gal1);
				galaxies_count := galaxies_count + 1;
			end if;
			bigx := bigx + 1;
		end loop;
		if integer_sets.contains(empty_lines, line_idx) then
			bigy := bigy + increment;
		end if;
		bigy := bigy + 1;
	end loop;

	for gal1_idx in 0 .. (galaxies_count - 1) loop
		gal1 := galaxies(gal1_idx);
		for gal2_idx in (gal1_idx + 1) .. (galaxies_count - 1) loop
			gal2 := galaxies(gal2_idx);
			result := result + (abs (gal1.x - gal2.x)) + (abs (gal1.y - gal2.y));
		end loop;
	end loop;

	ada.long_integer_text_io.put(result, width => 1);
end main;
