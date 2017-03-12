static inline int mandel(float c_re, float c_im) {
  {
    float z_re = c_re;
    float z_im = c_im;
    int ret = 0;

    for (unsigned int i = 0; (i < 256); i += 1) {
      {
        float re2 = (z_re * z_re);
        float im2 = (z_im * z_im);

        if (((4.e+0) < (re2 + im2))) {
          ret = i;
          break;
        }

        {
          float new_re = ((z_re * z_re) - (z_im * z_im));
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
                            uniform float dx, uniform float dy,
                            uniform int output[], uniform int output_row_start,
                            uniform int output_col_start,
                            uniform int output_row_end,
                            uniform int output_col_end) {
  for (uniform int j = output_row_start; (j < output_row_end); j += 1) {
    {
      float y = (y0 + (j * dy));
      unsigned int i0 = (j * 512);

      not processable
          : (foreach (i output_col_start output_col_end)(let(
                (x type float init(+x0(*i dx)))(index type int init(+i i0)))(
                setf(aref output index)(funcall mandel x y))))
    }
  }
}
