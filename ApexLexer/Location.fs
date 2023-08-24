type Location = {
    line : int;
    row : int;
    column : int;
}
let create line row column = {line; row; column}
let no_loc : Location = {line = 0; row = 0; column = 0}