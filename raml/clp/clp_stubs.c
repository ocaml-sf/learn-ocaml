/*
 * Copyright (C) 2012 Yosuke Onoue
 */

#include <stdio.h>
#include <stdlib.h>
#include <coin/Clp_C_Interface.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>


#define Model_val(v) (*((Clp_Simplex**)Data_custom_val(v)))


static void finalize_model(value v)
{
    Clp_deleteModel(Model_val(v));
}


static struct custom_operations clp_model_ops =
{
    "com.likr-lab.ocaml-clp.0",
    finalize_model,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};


static value alloc_model(Clp_Simplex* model)
{
    value block = caml_alloc_custom(&clp_model_ops, 1, 0, 1);
    Model_val(block) = model;
    return block;
}


/*
 * Creating model
 */


/*
 * val create : unit -> t;;
 */
CAMLprim value clp_create(value unit)
{
    CAMLparam1 (unit);

    CAMLreturn (alloc_model(Clp_newModel()));
}


/*
 * Getters and setters of problem parameters
 */


/*
 * val resize : t -> int -> int -> unit;;
 */
CAMLprim value clp_resize(value model, value num_rows, value num_columns)
{
    CAMLparam3 (model, num_rows, num_columns);

    Clp_Simplex* c_model = Model_val(model);
    int c_num_rows = Int_val(num_rows);
    int c_num_columns = Int_val(num_columns);
    Clp_resize(c_model, c_num_rows, c_num_columns);

    CAMLreturn (Val_unit);
}


/*
 * val number_rows : t -> int;;
 */
CAMLprim value clp_number_rows(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    int num_rows = Clp_numberRows(c_model);

    CAMLreturn (Val_int(num_rows));
}


/*
 * val number_columns : t -> int;;
 */
CAMLprim value clp_number_columns(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);

    CAMLreturn (Val_int(num_columns));
}


/*
 * val number_elements : t -> int;;
 */
CAMLprim value clp_number_elements(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    int num_elements = Clp_getNumElements(c_model);

    CAMLreturn (Val_int(num_elements));
}


/*
 * val direction : t -> direction;;
 */
CAMLprim value clp_direction(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    double c_direction = Clp_optimizationDirection(c_model);
    int direction = c_direction < 0 ? 0 : 1;

    CAMLreturn (Val_int(direction));
}


/*
 * val set_direction : t -> direction -> unit;;
 */
CAMLprim value clp_set_direction(value model, value direction)
{
    CAMLparam2 (model, direction);

    Clp_Simplex* c_model = Model_val(model);
    double c_direction = Int_val(direction) == 0 ? -1 : 1;
    Clp_setOptimizationDirection(c_model, c_direction);

    CAMLreturn (Val_unit);
}


/*
 * val add_rows : t -> row array -> unit;;
 */
CAMLprim value clp_add_rows(value model, value rows)
{
    CAMLparam2 (model, rows);
    CAMLlocal3 (row, elements, element);

    Clp_Simplex* c_model = Model_val(model);
    const int row_count = Wosize_val(rows);
    double* c_row_lower = (double*)malloc(sizeof(double) * row_count);
    double* c_row_upper = (double*)malloc(sizeof(double) * row_count);
    int* c_row_starts = (int*)malloc(sizeof(int) * (row_count + 1));
    int* c_columns;
    double* c_elements;
    int i, j, k, row_size;

    c_row_starts[0] = 0;
    for (i = 0; i < row_count; ++i) {
        row = Field(rows, i);
        c_row_lower[i] = Double_val(Field(row, 0));
        c_row_upper[i] = Double_val(Field(row, 1));
        elements = Field(row, 2);
        row_size = Wosize_val(elements);
        c_row_starts[i + 1] = c_row_starts[i] + row_size;
    }

    c_columns = (int*)malloc(sizeof(int) * c_row_starts[row_count]);
    c_elements = (double*)malloc(sizeof(double) * c_row_starts[row_count]);

    k = 0;
    for (i = 0; i < row_count; ++i) {
        row = Field(rows, i);
        elements = Field(row, 2);
        row_size = Wosize_val(elements);
        for (j = 0; j < row_size; ++j) {
            element = Field(elements, j);
            c_columns[k] = Int_val(Field(element, 0));
            c_elements[k] = Double_val(Field(element, 1));
            ++k;
        }
    }

    Clp_addRows(
            c_model, row_count,
            c_row_lower, c_row_upper,
            c_row_starts, c_columns, c_elements);

    free(c_row_lower);
    free(c_row_upper);
    free(c_row_starts);
    free(c_columns);
    free(c_elements);

    CAMLreturn (Val_unit);
}


/*
 * val delete_rows : t -> int array -> unit;;
 */
CAMLprim value clp_delete_rows(value model, value which)
{
    CAMLparam2 (model, which);

    Clp_Simplex* c_model = Model_val(model);
    const int number = Wosize_val(which);
    int* c_which = (int*)malloc(sizeof(int) * number);
    int i;

    for (i = 0; i < number; ++i) {
        c_which[i] = Int_val(Field(which, i));
    }
    Clp_deleteRows(c_model, number, c_which);
    free(c_which);

    CAMLreturn (Val_unit);
}


/*
 * val add_columns : t -> column array -> unit;;
 */
CAMLprim value clp_add_columns(value model, value columns)
{
    CAMLparam2 (model, columns);
    CAMLlocal3 (column, elements, element);

    Clp_Simplex* c_model = Model_val(model);
    const int column_count = Wosize_val(columns);
    double* c_column_lower = (double*)malloc(sizeof(double) * column_count);
    double* c_column_upper = (double*)malloc(sizeof(double) * column_count);
    double* c_objective = (double*)malloc(sizeof(double) * column_count);
    int* c_column_starts = (int*)malloc(sizeof(int) * (column_count + 1));
    int* c_rows;
    double* c_elements;
    int i, j, k, column_size;

    c_column_starts[0] = 0;
    for (i = 0; i < column_count; ++i) {
        column = Field(columns, i);
        c_objective[i] = Double_val(Field(column, 0));
        c_column_lower[i] = Double_val(Field(column, 1));
        c_column_upper[i] = Double_val(Field(column, 2));
        elements = Field(column, 3);
        column_size = Wosize_val(elements);
        c_column_starts[i + 1] = c_column_starts[i] + column_size;
    }

    c_rows = (int*)malloc(sizeof(int) * c_column_starts[column_count]);
    c_elements = (double*)malloc(sizeof(double) * c_column_starts[column_count]);

    k = 0;
    for (i = 0; i < column_count; ++i) {
        column = Field(columns, i);
        elements = Field(column, 3);
        column_size = Wosize_val(elements);
        for (j = 0; j < column_size; ++j) {
            element = Field(elements, j);
            c_rows[k] = Int_val(Field(element, 0));
            c_elements[k] = Double_val(Field(element, 1));
            ++k;
        }
    }

    Clp_addColumns(
            c_model, column_count,
            c_column_lower, c_column_upper, c_objective,
            c_column_starts, c_rows, c_elements);

    free(c_column_lower);
    free(c_column_upper);
    free(c_objective);
    free(c_column_starts);
    free(c_rows);
    free(c_elements);

    CAMLreturn (Val_unit);
}


/*
 * val delete_columns : t -> int array -> unit;;
 */
CAMLprim value clp_delete_columns(value model, value which)
{
    CAMLparam2 (model, which);

    Clp_Simplex* c_model = Model_val(model);
    const int number = Wosize_val(which);
    int* c_which = (int*)malloc(sizeof(int) * number);
    int i;

    for (i = 0; i < number; ++i) {
        c_which[i] = Int_val(Field(which, i));
    }
    Clp_deleteColumns(c_model, number, c_which);
    free(c_which);

    CAMLreturn (Val_unit);
}


/*
 * val row_lower : t -> float array;;
 */
CAMLprim value clp_row_lower(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (row_lower);

    Clp_Simplex* c_model = Model_val(model);
    int num_rows = Clp_numberRows(c_model);
    double* c_row_lower = Clp_rowLower(c_model);
    int i;

    row_lower = caml_alloc(num_rows, Double_array_tag);
    for (i = 0; i < num_rows; ++i) {
        Store_double_field(row_lower, i, c_row_lower[i]);
    }

    CAMLreturn (row_lower);
}


/*
 * val change_row_lower : t -> float array -> unit;;
 */
CAMLprim value clp_change_row_lower(value model, value row_lower)
{
    CAMLparam2 (model, row_lower);

    Clp_Simplex* c_model = Model_val(model);
    int num_rows = Clp_numberRows(c_model);
    double* c_row_lower = (double*)malloc(sizeof(double) * num_rows);
    int i;

    for (i = 0; i < num_rows; ++i) {
        c_row_lower[i] = Double_field(row_lower, i);
    }
    Clp_chgRowLower(c_model, c_row_lower);
    free(c_row_lower);

    CAMLreturn (Val_unit);
}


/*
 * val row_upper : t -> float array;;
 */
CAMLprim value clp_row_upper(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (row_upper);

    Clp_Simplex* c_model = Model_val(model);
    int num_rows = Clp_numberRows(c_model);
    double* c_row_upper = Clp_rowUpper(c_model);
    int i;

    row_upper = caml_alloc(num_rows, Double_array_tag);
    for (i = 0; i < num_rows; ++i) {
        Store_double_field(row_upper, i, c_row_upper[i]);
    }

    CAMLreturn (row_upper);
}


/*
 * val change_row_upper : t -> float array -> unit;;
 */
CAMLprim value clp_change_row_upper(value model, value row_upper)
{
    CAMLparam2 (model, row_upper);

    Clp_Simplex* c_model = Model_val(model);
    int num_rows = Clp_numberRows(c_model);
    double* c_row_upper = (double*)malloc(sizeof(double) * num_rows);
    int i;

    for (i = 0; i < num_rows; ++i) {
        c_row_upper[i] = Double_field(row_upper, i);
    }
    Clp_chgRowUpper(c_model, c_row_upper);
    free(c_row_upper);

    CAMLreturn (Val_unit);
}


/*
 * val column_lower : t -> float array;;
 */
CAMLprim value clp_column_lower(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (column_lower);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);
    double* c_column_lower = Clp_columnLower(c_model);
    int i;

    column_lower = caml_alloc(num_columns, Double_array_tag);
    for (i = 0; i < num_columns; ++i) {
        Store_double_field(column_lower, i, c_column_lower[i]);
    }

    CAMLreturn (column_lower);
}


/*
 * val change_column_lower : t -> float array -> unit;;
 */
CAMLprim value clp_change_column_lower(value model, value column_lower)
{
    CAMLparam2 (model, column_lower);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);
    double* c_column_lower = (double*)malloc(sizeof(double) * num_columns);
    int i;

    for (i = 0; i < num_columns; ++i) {
        c_column_lower[i] = Double_field(column_lower, i);
    }
    Clp_chgColumnLower(c_model, c_column_lower);
    free(c_column_lower);

    CAMLreturn (Val_unit);
}


/*
 * val column_upper : t -> float array;;
 */
CAMLprim value clp_column_upper(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (column_upper);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);
    double* c_column_upper = Clp_columnUpper(c_model);
    int i;

    column_upper = caml_alloc(num_columns, Double_array_tag);
    for (i = 0; i < num_columns; ++i) {
        Store_double_field(column_upper, i, c_column_upper[i]);
    }

    CAMLreturn (column_upper);
}


/*
 * val change_column_upper : t -> float array -> unit;;
 */
CAMLprim value clp_change_column_upper(value model, value column_upper)
{
    CAMLparam2 (model, column_upper);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);
    double* c_column_upper = (double*)malloc(sizeof(double) * num_columns);
    int i;

    for (i = 0; i < num_columns; ++i) {
        c_column_upper[i] = Double_field(column_upper, i);
    }
    Clp_chgColumnUpper(c_model, c_column_upper);
    free(c_column_upper);

    CAMLreturn (Val_unit);
}


/*
 * val objective_coefficients : t -> float array;;
 */
CAMLprim value clp_objective_coefficients(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (objective_coefficients);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);
    double* c_objective_coefficients = Clp_objective(c_model);
    int i;

    objective_coefficients = caml_alloc(num_columns, Double_array_tag);
    for (i = 0; i < num_columns; ++i) {
        Store_double_field(objective_coefficients, i, c_objective_coefficients[i]);
    }

    CAMLreturn (objective_coefficients);
}


/*
 * val change_objective_coefficients : t -> float array -> unit;;
 */
CAMLprim value clp_change_objective_coefficients(
        value model, value objective_coefficients)
{
    CAMLparam2 (model, objective_coefficients);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);
    double* c_objective_coefficients = (double*)malloc(sizeof(double) * num_columns);
    int i;

    for (i = 0; i < num_columns; ++i) {
        c_objective_coefficients[i] = Double_field(objective_coefficients, i);
    }
    Clp_chgObjCoefficients(c_model, c_objective_coefficients);
    free(c_objective_coefficients);

    CAMLreturn (Val_unit);
}


/*
 * Getters and setters of solver parameters
 */


/*
 * val log_level : t -> int;;
 */
CAMLprim value clp_log_level(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    int c_log_level = Clp_logLevel(c_model);

    CAMLreturn (Val_int(c_log_level));
}


/*
 * val set_log_level : t -> int -> unit;;
 */
CAMLprim value clp_set_log_level(value model, value log_level)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    Clp_setLogLevel(c_model, Int_val(log_level));

    CAMLreturn (Val_unit);
}

/*
 * Solver oprations
 */

/*
 * val primal : t -> unit;;
 */
CAMLprim value clp_primal(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    Clp_primal(c_model, 0);

    CAMLreturn (Val_unit);
}


/*
 * val dual : t -> unit;;
 */
CAMLprim value clp_dual(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    Clp_dual(c_model, 0);

    CAMLreturn (Val_unit);
}


/*
 * Retrieving solutions
 */


/*
 * val objective_value : t -> float;;
 */
CAMLprim value clp_objective_value(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);

    CAMLreturn (caml_copy_double(Clp_objectiveValue(c_model)));
}


/*
 * val primal_row_solution : t -> float array;;
 */
CAMLprim value clp_primal_row_solution(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (solution);

    Clp_Simplex* c_model = Model_val(model);
    int num_rows = Clp_numberRows(c_model);
    double* c_solution = Clp_primalRowSolution(c_model);
    int i;

    solution = caml_alloc(num_rows, Double_array_tag);
    for (i = 0; i < num_rows; ++i) {
        Store_double_field(solution, i, c_solution[i]);
    }

    CAMLreturn (solution);
}


/*
 * val primal_column_solution : t -> float array;;
 */
CAMLprim value clp_primal_column_solution(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (solution);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);
    double* c_solution = Clp_primalColumnSolution(c_model);
    int i;

    solution = caml_alloc(num_columns, Double_array_tag);
    for (i = 0; i < num_columns; ++i) {
        Store_double_field(solution, i, c_solution[i]);
    }

    CAMLreturn (solution);
}


/*
 * val dual_row_solution : t -> float array;;
 */
CAMLprim value clp_dual_row_solution(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (solution);
    Clp_Simplex* c_model = Model_val(model);
    int num_rows = Clp_numberRows(c_model);
    double* c_solution = Clp_dualRowSolution(c_model);
    int i;

    solution = caml_alloc(num_rows, Double_array_tag);
    for (i = 0; i < num_rows; ++i) {
        Store_double_field(solution, i, c_solution[i]);
    }

    CAMLreturn (solution);
}


/*
 * val dual_column_solution : t -> float array;;
 */
CAMLprim value clp_dual_column_solution(value model)
{
    CAMLparam1 (model);
    CAMLlocal1 (solution);

    Clp_Simplex* c_model = Model_val(model);
    int num_columns = Clp_numberColumns(c_model);
    double* c_solution = Clp_dualColumnSolution(c_model);
    int i;

    solution = caml_alloc(num_columns, Double_array_tag);
    for (i = 0; i < num_columns; ++i) {
        Store_double_field(solution, i, c_solution[i]);
    }

    CAMLreturn (solution);
}

/*
 * val status : t -> int;;
 */
CAMLprim value clp_status(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    int c_status = Clp_status(c_model);

    CAMLreturn (Val_int(c_status));    
}


/*
 * val initial_solve : t -> unit;;
 */
CAMLprim value clp_initial_solve(value model)
{
    CAMLparam1 (model);

    Clp_Simplex* c_model = Model_val(model);
    Clp_initialSolve(c_model);

    CAMLreturn (Val_unit);
}
