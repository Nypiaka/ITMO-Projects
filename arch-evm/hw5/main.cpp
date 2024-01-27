#include <iostream>
#include <chrono>
#include "omp.h"

using namespace std;

int main(int var, char **files) {

    int numOfThr = atoi(files[1]);
    int type;
    if (numOfThr != 0)
        omp_set_num_threads(numOfThr);
    else omp_set_num_threads(8);
    FILE *fin = fopen(files[2], "rb");
    if (!fin) {
        cerr << "File not found";
        return -1;
    }
    FILE *fout = fopen(files[3], "wb");
    size_t height;
    size_t length;
    double coefficient = stod((string) (files[4]));
    char e;
    char e1;
    fprintf(fout, "%c", 'P');
    e1 = getc(fin);
    e = getc(fin);
    if (e1 != 'P' || (e != '5' && e != '6')) {
        cerr << "Wrong file format";
        return -1;
    }
    int numOfChannels;
    if (e == 5) { numOfChannels = 1; }
    else numOfChannels = 3;
    fprintf(fout, "%c\n", e);
    fscanf(fin, "%d %d\n", &length, &height);
    fprintf(fout, "%d %d\n", length, height);
    fscanf(fin, "%d\n", &type);
    if (type != 255) {
        cerr << "Wrong file format";
        return -1;
    }
    fprintf(fout, "%d\n", type);
    auto *base = new size_t[numOfChannels * 256];
    int *mas = new int[numOfChannels * height * length];
    for (int i = 0; i < numOfChannels * 256; i++) {
        base[i] = 0;
    }
    unsigned char a;
    for (size_t i = 0; i <= height * length * numOfChannels; i++) {
        a = getc(fin);
        mas[i] = ((int) a);//сам ввод
        base[((int) a) + (i % numOfChannels) * 256] += 1;//цифровая сортировка

    }
    if (!feof(fin)) {
        cerr << "Wrong resolution";
        return -1;
    }
    auto begin = chrono::steady_clock::now();//старт замера
    size_t right;
    size_t left;
    left = (size_t) (height * length * coefficient);//граница слева
    right = (size_t) (height * length * (1 - coefficient));//набор справа
    for (int i = 0; i < numOfChannels; i++) {
        int max;
        int min;
        size_t sumax = 0;
        size_t sumin = 0;
        int j1 = 0;
        while (sumax < right) { //поиск граничного значения
            sumax += base[i * 256 + j1];
            j1++;
        }
        max = j1;
        j1 = 0;
        while (sumin < left) { //поиск граничного значения
            sumin += base[i * 256 + j1];
            j1++;
        }
        min = j1;

#pragma omp parallel for schedule(static) //тут можно изменять параметр. собственно сам омп
        for (int j = i; j < height * length * numOfChannels; j += numOfChannels) {
            if (max - min != 0) {
                mas[j] = 255 * (mas[j] - min) / (max - min);
                if (mas[j] > 255) { mas[j] = 255; }
                if (mas[j] < 0) { mas[j] = 0; }
            }
        }
    }
    auto end = chrono::steady_clock::now();//окончание замеров
    printf("Time (%i thread(s)): %d ms\n", numOfThr,
           chrono::duration_cast<std::chrono::milliseconds>(end - begin).count());
    for (int i = 0; i < numOfChannels * height * length; i++) {
        fwrite(&mas[i], sizeof(char), 1, fout);//вывод
    }
    fclose(fin);
    fclose(fout);
    return 0;
}