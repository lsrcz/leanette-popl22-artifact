#!/bin/env python3

import csv
import math

projectname = {'nanoscala': 'NanoScala',
               'letpoly': 'LetPoly',
               'regex': 'Regex',
               'ferrite': 'Ferrite',
               'fluidics': 'Fluidics',
               'ifcl': 'IFCL',
               'cosette': 'Cosette',
               'cosette-1': 'Cosette-1',
               }


def read_file(filename):
    ret = {}
    with open(filename, newline='') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            ret[row['subject']] = row
    return ret


def format_number(number):
    if number > 1:
        return '{:.1f}'.format(number)
    else:
        return '{:.2g}'.format(number)


def print_table2(grisette, r3, r4):
    print(r'\begin{tabular}{lrrrrrrrrrrrrrr}')
    print(r'  \hline')
    print(r'  \multirow{2}{*}{Benchmark}&\multicolumn{4}{c}{Grisette}&&\multicolumn{4}{c}{Rosette 3}&&\multicolumn{4}{c}{Rosette 4}\\')
    print(r'  \cline{2-5}\cline{7-10}\cline{12-15}')
    print(r'  &Tot  &Eval &Solv &Term  &&Tot    &Eval  &Solv   &Term   &&Tot    &Eval  &Solv   &Term   \\')
    print(r'  \hline')
    for project in ['ferrite', 'ifcl', 'fluidics', 'cosette', 'nanoscala', 'letpoly', 'cosette-1']:
        if project == 'cosette-1':
            print(r'  \hline')
        tot_g = float(grisette[project]['total'])
        eval_g = float(grisette[project]['eval'])
        solv_g = float(grisette[project]['solv'])
        term_g = float(grisette[project]['term']) / 1000

        tot_r3 = float(r3[project]['total']) / 1000
        eval_r3 = float(r3[project]['eval']) / 1000
        solv_r3 = float(r3[project]['solv']) / 1000
        term_r3 = float(r3[project]['term']) / 1000

        tot_r4 = float(r4[project]['total']) / 1000
        eval_r4 = float(r4[project]['eval']) / 1000
        solv_r4 = float(r4[project]['solv']) / 1000
        term_r4 = float(r4[project]['term']) / 1000

        print('  \\textsc{{{}}} &{}&{}&{}&{}&&{}&{}&{}&{}&&{}&{}&{}&{}\\\\'.format(
            projectname[project],
            format_number(tot_g),
            format_number(eval_g),
            format_number(solv_g),
            format_number(term_g),
            format_number(tot_r3),
            format_number(eval_r3),
            format_number(solv_r3),
            format_number(term_r3),
            format_number(tot_r4),
            format_number(eval_r4),
            format_number(solv_r4),
            format_number(term_r4),
        ))
    print(r'  \hline')
    print(r'\end{tabular}')


def geomean(xs):
    return math.exp(math.fsum(math.log(x) for x in xs) / len(xs))


def print_table3(grisette, r3, r4):
    print(r'\begin{tabular}{lrrrrrrrrrrrrrrr}')
    print(r'  \hline')
    print(r'  \multirow{2}{*}{Version}&\multicolumn{3}{c}{Total time}&&\multicolumn{3}{c}{Eval time}&&\multicolumn{3}{c}{Solve time}&&\multicolumn{3}{c}{Term count}\\')
    print(r'  \cline{2-4}\cline{6-8}\cline{10-12}\cline{14-16}')
    print(r'  &best  &worst &mean  &&best  &worst &mean  &&best  &worst &mean  &&best  &worst &mean  \\')
    print(r'  \hline')
    tot_l = {'grisette': [], 'r3': [], 'r4': []}
    eval_l = {'grisette': [], 'r3': [], 'r4': []}
    solv_l = {'grisette': [], 'r3': [], 'r4': []}
    term_l = {'grisette': [], 'r3': [], 'r4': []}
    metrics = {'total': tot_l, 'eval': eval_l, 'solv': solv_l, 'term': term_l}
    tbls = {'grisette': grisette, 'r3': r3, 'r4': r4}
    for project in ['ferrite', 'ifcl', 'fluidics', 'cosette', 'nanoscala', 'letpoly', 'cosette-1']:
        for m in metrics:
            for t in tbls:
                value = float(tbls[t][project][m])
                if m != 'term' and t != 'grisette':
                    value /= 1000
                metrics[m][t].append(value)

    for t in ['r3', 'r4']:
        if t == 'r3':
            print('  Rosette 3', end='')
        else:
            print('  Rosette 4', end='')
        for m in metrics:
            if m != 'total':
                print('&', end='')
            if m != 'term':
                l = list(
                    map(lambda p: p[1] / p[0], zip(metrics[m]['grisette'], metrics[m][t])))
                best = max(*l)
                worst = min(*l)
                mean = geomean(l)
                print('&{:.1f}x&{:.1f}x&{:.1f}x'.format(
                    best, worst, mean), end='')
            else:
                l = list(
                    map(lambda p: p[0] / p[1], zip(metrics[m]['grisette'], metrics[m][t])))
                best = min(*l)
                worst = max(*l)
                mean = geomean(l)
                print('&{:.3g}\\%&{:.3g}\\%&{:.3g}\\%'.format(
                    best * 100, worst * 100, mean * 100), end='')
        print('\\\\')
    print(r'  \hline')
    print(r'\end{tabular}')


def print_table4(grisette):
    print(r'\begin{tabular}{lrrrrrrrrrrrr}')
    print(r'  \hline')
    print(r'  \multirow{2}{*}{Benchmark}&\multicolumn{4}{c}{no memoization}&&\multicolumn{4}{c}{with memoization}&&\multicolumn{2}{c}{speedup ratio}\\')
    print(r'  \cline{2-5}\cline{7-10}\cline{12-13}')
    print(r'  &Total &Eval &Lower &Solv &&Total &Eval &Lower &Solv &&Total &Eval \\')
    print(r'  \hline')

    for project in ['nanoscala', 'letpoly', 'regex']:
        tot_no = float(grisette[project + '-nomemo']['total'])
        eval_no = float(grisette[project + '-nomemo']['pureeval'])
        lower_no = float(grisette[project + '-nomemo']['lower'])
        solv_no = float(grisette[project + '-nomemo']['solv'])
        tot_memo = float(grisette[project]['total'])
        eval_memo = float(grisette[project]['pureeval'])
        lower_memo = float(grisette[project]['lower'])
        solv_memo = float(grisette[project]['solv'])
        ratio_tot = tot_no / tot_memo
        ratio_eval = eval_no / eval_memo
        print('  \\textsc{{{}}} &{}&{}&{}&{}&&{}&{}&{}&{}&&{}x&{}x\\\\'.format(
            projectname[project],
            format_number(tot_no),
            format_number(eval_no),
            format_number(lower_no),
            format_number(solv_no),
            format_number(tot_memo),
            format_number(eval_memo),
            format_number(lower_memo),
            format_number(solv_memo),
            format_number(ratio_tot),
            format_number(ratio_eval),
        ))

    print(r'  \hline')
    print(r'\end{tabular}')


if __name__ == '__main__':
    grisette = read_file('grisette.csv')
    r3 = read_file('r3.csv')
    r4 = read_file('r4.csv')
    print_table2(grisette, r3, r4)
    print_table3(grisette, r3, r4)
    print_table4(grisette)
