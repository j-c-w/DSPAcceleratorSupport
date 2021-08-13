import argparse
import matplotlib.pyplot as plt
import plot
import plot_ffta
import numpy as np

def plot_graph(fftw_results, ffta_results, powerquad_results):
    gridspec = {
            'width_ratios': [len(x) for x in fftw_results]
            }
    fix, (ax1, ax2, ax3) = plt.subplots(1, len(fftw_results), figsize=(8,3), gridspec_kw = gridspec)
    ax3.set_yscale('log', basey=10)
    ax3.set_ylim([0.1, 100000])

    axes = [ax1, ax2, ax3]

    last_maxx = 0
    for i in range(0, len(fftw_results)):
        fftw_group = fftw_results[i]
        ffta_group = ffta_results[i]
        powerquad_group = powerquad_results[i]
        axi = axes[i]
        minx = last_maxx
        maxx = minx + len(fftw_group)
        last_maxx = maxx

        xpos = np.arange(0, len(fftw_group))
        print (len(ffta_group))
        print (len(fftw_group))
        print (len(xpos))
        width = 0.25
        axi.bar(xpos, ffta_group, width, label='FFTA', color='green', hatch='-')
        axi.bar(xpos + width, fftw_group,  width,  label='FFTW', color='orange')
        axi.bar(xpos + width * 2, powerquad_group, width, label='PowerQuad', color='red', hatch='.')
        axi.set_xticks(xpos + width)
        axi.set_xticklabels(range(minx, maxx))

    ax2.legend()
    ax2.set_xlabel('Project Number')
    ax1.set_ylabel('Relative Performance')

    plt.tight_layout()
    plt.savefig('barplot_speedup.eps')

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('size', type=int)
    parser.add_argument('directories', nargs='+')
    args = parser.parse_args()

    ffta_results = []
    fftw_results = []
    powerquad_results = []

    i = 0
    for folder in args.directories:
        print ("Folder " + folder)
        print ("is project " + str(i))
        i += 1
        # Get the FFTW results:
        fftw_orig = plot.load_result_maps(folder, 'accelerated_fftw_orig_results', "json_out")
        fftw_acc = plot.load_result_maps(folder, 'accelerated_fftw_results', "json_out")

        if args.size not in fftw_orig:
            size = max(fftw_orig.keys())
            print("For project failed to get the sizes")
            print (folder)
            print ("Using size ", size)
        else:
            size = args.size

        fftw_results.append(fftw_orig[size] / fftw_acc[size])
        print ("Value used is ", fftw_results[-1])

        # Get the FFTA results:
        ffta_orig, _ = plot_ffta.read_file(folder + "/ffta_results/" + "UnAcceleratedResults")
        ffta_acc, _ = plot_ffta.read_file(folder + "/ffta_results/" + "AcceleratedResults")

        if args.size not in ffta_orig:
            size = max(ffta_orig.keys())
            print("For project failed to get the sizes")
            print (folder)
            print ("Using size ", size)
        else:
            size = args.size

        ffta_results.append(np.median(ffta_orig[size] / ffta_acc[size]))

        # Get PowerQuad results:
        powerquad_orig, _ = plot_ffta.read_file(folder + "/powerquad_results/UnAcceleratedResults")
        powerquad_acc, _ = plot_ffta.read_file(folder + "/powerquad_results/AcceleratedResults")

        if len(powerquad_orig) > 0:
            if args.size not in powerquad_orig:
                size = max(powerquad_orig.keys())
                print ("For project failed to get the sizes")
                print (folder)
                print ("Using size ", size)
            else:
                size = args.size

            powerquad_results.append(np.median(powerquad_orig[size] / powerquad_acc[size]))
            print ("Value used is ", ffta_results[-1])
        else:
            powerquad_results.append(0.0)


    # Sort the values:
    res = []
    for i in range(len(args.directories)):
        res.append(
                (
                ffta_results[i],
                fftw_results[i],
                powerquad_results[i],
                args.directories[i]
                )
        )

    res = sorted(res)

    fftw_results = []
    ffta_results = []
    powerquad_results = []
    dirs = []
    i = 0
    for r in res:
        if i == 0 or i == 8 or i == 13:
            # That's where the new groups start --- a better way to do this would be to compute the actual thresholds that are being used for this division.
            ffta_results.append([])
            fftw_results.append([])
            powerquad_results.append([])
            dirs.append([])

        ffta_results[-1].append(r[0])
        fftw_results[-1].append(r[1])
        powerquad_results[-1].append(r[2])
        dirs[-1].append(r[3])

        i += 1

    plot_graph(fftw_results, ffta_results, powerquad_results)
    print ("Order of folders")
    print (dirs)
