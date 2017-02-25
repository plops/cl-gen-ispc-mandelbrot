static inline int mandel(float c_re, float c_im) {
  {
    float z_re = c_re;
    float z_im = c_im;
    const int count = 256;
    int ret = 0;

    for (int i = 0; (i < count); i += 1) {
      {
        float re2 = (z_re * z_re);
        float im2 = (z_im * z_im);

        if (((4.e+0) < (re2 + im2))) {
          ret = i;
          break;
        }

        {
          float new_re = (re2 - im2);
          float new_im = ((2.e+0) * z_re * z_im);

          z_re = (c_re + new_re);
          z_im = (c_im + new_im);
        }
      }
    }

    return ret;
  }
}

export void mandelbrot_ispc(uniform float x0, uniform float y0,
                            uniform float x1, uniform float y1,
                            uniform int height, uniform int output[]) {
  {
    float dx = ((x1 - x0) / 768);
    float dy = ((y1 - y0) / height);

    for (uniform int j = 0; (j < height); j += 1) {
      foreach (i = 0 ... 768) {
        {
          float x = (x0 + (i * dx));
          float y = (y0 + (i * dy));
          int index = (i + (j * 768));

          output[index] = mandel(x, y);
        }
      }
    }
  }
}
