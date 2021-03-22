unsigned int reverseBits(unsigned int num, unsigned int no_bits)
{
    unsigned int reverse_num = 0, temp;
	unsigned int i = 0;

	while (no_bits > 1)
    {
        temp = (num & (1 << i));
        if(temp)
            reverse_num |= (1 << (no_bits));
		no_bits = no_bits >> 1;
		i ++;
    }

    return reverse_num;
}
