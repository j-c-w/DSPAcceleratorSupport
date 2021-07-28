import matplotlib.pyplot as plt
import argparse
import os
import numpy as np
import re


def plot(ctimes):
    plt.clf()
    for time in ctimes:
        x, y = time.get_cdf()
        plt.plot(x, y, label=time.name)

    plt.legend()
    axes = plt.gca()
    axes.set_ylim([0.0, 1.001])
    axes.set_xscale('log')
    
    plt.xlabel('Compile Time (s)')
    plt.ylabel('CDF')

    plt.savefig('compile_times.eps')

def parse_time(time_string):
    if 'm' in time_string:
        split_times = time_string.split('m')
        minutes = int(split_times[0])
        rest_time_string = split_times[1]
    else:
        minutes = 0

    if 's' in time_string:
        seconds = float(rest_time_string.split('s')[0])
    else:
        seconds = 0.0

    return (60.0 * minutes) + seconds


class CTimes(object):
    def __init__(self, name, path):
        self.name = name
        self.path = path

    def get_times(self):
        times = []
        with open(self.path) as f:
            for line in f.readlines():
                if 'real\t' in line:
                    time = parse_time(re.split('\t', line)[1])
                    times.append(time)
        return times

    def get_cdf(self):
        times = np.array(sorted(self.get_times()))
        y_var = np.array(range(0, len(times)))
        y_var = y_var / (len(times) - 1) # normalize y axis.

        return times, y_var

if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument('folder')
    args = parser.parse_args()

    ctimes_files = []
    for path, sdirs, files in os.walk(args.folder):
        for name in files:
            pname = os.path.join(path, name)
            ctimes_files.append(CTimes(name, pname))

    plot(ctimes_files)
