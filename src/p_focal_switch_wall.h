#ifndef P_FOCAL_SWITCH_WALL_H
#define P_FOCAL_SWITCH_WALL_H

#include "p_focal.h"

#include <cstdlib>

//generated using this python3 code

/*
#!/bin/python3

def populate_switch_tree(layers, f_name, arg_string, tab="    ", tab_depth=1, picks=[]):
    if(len(layers)):
        out = f'{tab*tab_depth}switch({layers[0][0]}){"{"}\n'
        for pick in layers[0][2]:
            out += f'{tab*(tab_depth+1)}break; case {layers[0][1]}::{pick}:\n'
            out += populate_switch_tree(layers[1:], f_name, arg_string, tab, tab_depth+2, picks+[f'{layers[0][1]}::{pick}'])
        out += f'{tab*(tab_depth+1)}break; default: std::cerr << "bad {layers[0][0]}\\n";exit(64);\n' #exit(64); is the bsd exit value EX_USAGE, meaning that the arugments are wrong
        out += f'{tab*(tab_depth)}{"}"}\n'
        return out
    else:
        return f'{tab*tab_depth}{f_name}<{", ".join(picks)}>{arg_string};\n'

layers = (
    ("tf","p_focal::TRANSFORM",("MULTIPLY", "ADD", "R_EXP", "L_EXP")),
    ("rf","p_focal::REDUCE",("SUM", "PRODUCT", "MIN", "MAX", "MEAN", "VARIANCE")),
    ("nf","p_focal::NAN_POLICY",("NOTHING_SPECIAL", "SKIP_NAN_IN_KERNEL", "SKIP_NAN_IN_DATA", "SKIP_NAN_IN_KERNEL_OR_DATA", "SKIP_NAN_IN_INTERMEDIATE")),
    ("mf","p_focal::MEAN_POLICY",("KENEL_SIZE", "SUM_NON_NAN_KERNEL_VALUES", "MUL_NON_NAN_KERNEL_VALUES", "COUNT_NON_NAN_KERNEL_VALUES", "COUNT_NON_NAN_INTERMEDIATE_VALUES"))
    )


print("#ifndef P_FOCAL_SWITCH_WALL_H")
print("#define P_FOCAL_SWITCH_WALL_H")
print("")
print('#include "p_focal.h"')
print("")
print("#include <cstdlib>")
print("")
print("//generated using this python3 code")
print("")
print("/"+"*")
with open(__file__) as this_file:
    print(this_file.read())
print("*"+"/")
print("")
print("inline void p_focal_switch_wall(")
print("        const p_focal::expanded_aligned_data<>& src,")
print("        const p_focal::expanded_aligned_data<>& k,")
print("        double* dest,")
print("\n".join(f"        {l[1]} {l[0]}," for l in layers))
print("        const bool open_mp_enabled=_P_FOCAL_OPENMP_ENADLED){")
print(populate_switch_tree(layers, "p_focal::p_conv", "(src, k, dest, open_mp_enabled)") + "}")
print("")
print("#endif /"+"*P_FOCAL_SWITCH_WALL_H*"+"/")

*/

inline void p_focal_switch_wall(
        const p_focal::expanded_aligned_data<>& src,
        const p_focal::expanded_aligned_data<>& k,
        double* dest,
        p_focal::TRANSFORM tf,
        p_focal::REDUCE rf,
        p_focal::NAN_POLICY nf,
        p_focal::MEAN_POLICY mf,
        const bool open_mp_enabled=_P_FOCAL_OPENMP_ENADLED){
    switch(tf){
        break; case p_focal::TRANSFORM::MULTIPLY:
            switch(rf){
                break; case p_focal::REDUCE::SUM:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::PRODUCT:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MIN:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MAX:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MEAN:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::VARIANCE:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::MULTIPLY, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; default: std::cerr << "bad rf\n";exit(64);
            }
        break; case p_focal::TRANSFORM::ADD:
            switch(rf){
                break; case p_focal::REDUCE::SUM:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::PRODUCT:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MIN:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MAX:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MEAN:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::VARIANCE:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::ADD, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; default: std::cerr << "bad rf\n";exit(64);
            }
        break; case p_focal::TRANSFORM::R_EXP:
            switch(rf){
                break; case p_focal::REDUCE::SUM:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::PRODUCT:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MIN:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MAX:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MEAN:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::VARIANCE:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::R_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; default: std::cerr << "bad rf\n";exit(64);
            }
        break; case p_focal::TRANSFORM::L_EXP:
            switch(rf){
                break; case p_focal::REDUCE::SUM:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::SUM, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::PRODUCT:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::PRODUCT, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MIN:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MIN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MAX:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MAX, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::MEAN:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::MEAN, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; case p_focal::REDUCE::VARIANCE:
                    switch(nf){
                        break; case p_focal::NAN_POLICY::NOTHING_SPECIAL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::NOTHING_SPECIAL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_KERNEL_OR_DATA, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; case p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE:
                            switch(mf){
                                break; case p_focal::MEAN_POLICY::KENEL_SIZE:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::KENEL_SIZE>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::SUM_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::MUL_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_KERNEL_VALUES>(src, k, dest, open_mp_enabled);
                                break; case p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES:
                                    p_focal::p_conv<p_focal::TRANSFORM::L_EXP, p_focal::REDUCE::VARIANCE, p_focal::NAN_POLICY::SKIP_NAN_IN_INTERMEDIATE, p_focal::MEAN_POLICY::COUNT_NON_NAN_INTERMEDIATE_VALUES>(src, k, dest, open_mp_enabled);
                                break; default: std::cerr << "bad mf\n";exit(64);
                            }
                        break; default: std::cerr << "bad nf\n";exit(64);
                    }
                break; default: std::cerr << "bad rf\n";exit(64);
            }
        break; default: std::cerr << "bad tf\n";exit(64);
    }
}

#endif /*P_FOCAL_SWITCH_WALL_H*/
