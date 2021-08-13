import argparse
import matplotlib.pyplot as plt

def parse_reason(r):
    # break up longer things into multiple lines
    words = r.split(' ')
    res = ""
    i = 1
    for word in words:
        i += 1
        res = res + word + " "
        if (i % 2) == 0:
            print ("adding nline")
            res += "\n"
    print(res)
    return res


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('InfoFile')
    parser.add_argument('BrokenFile')
    args = parser.parse_args()

    total_size = 0
    with open(args.InfoFile) as f:
        reasons = {}
        for line in f.readlines():
            print (line)
            reason = parse_reason(line.split(":")[1].strip())
            if reason in reasons:
                reasons[reason] += 1
                total_size += 1
            else:
                reasons[reason] = 1
                total_size += 1
    with open(args.BrokenFile) as f:
        for line in f.readlines():
            print(line)
            reason = parse_reason(line.split(":")[1].strip())
            if reason in reasons:
                reasons[reason] += 1
                total_size += 1
            else:
                reasons[reason] = 1
                total_size += 1

    sizes = []
    explode = []
    labels = []

    colors = [
            # May need more?
            'pink', 'blue', 'orange', 'green', 'red', 'purple', 'brown'
            ]
    values = []
    # sort everthing into smallers -> greates
    for key in reasons.keys():
        values.append( (
            float(reasons[key]) / (float(total_size)),
            key
            )
        )

    values = sorted(values)
    sizes = [v[0] for v in values]
    labels = [v[1] for v in values]

    fig, ax = plt.subplots()
    ax.bar(range(0, len(labels)), sizes, color=colors, width=0.5)
    plt.xticks(rotation=90)
    plt.ylabel("Fraction of Benchmarks")
    # plt.ylim([0, 1])
    ax.set_xticklabels([''] + labels)
    plt.tight_layout()
    # ax.pie(sizes, explode=explode, labels=labels, autopct='%1.f%%', shadow=True, startangle=90)
    # ax.axis('equal')

    
    # plt.legend()
    # plt.gcf().subplots_adjust(left=0.34)
    plt.savefig('open_source_project_distribution.eps')
