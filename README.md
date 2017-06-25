# ASCIIART
make ASCII ART from BMP file

元プログラムを、fortran66 さんのここからこころよく提供いただきました。
http://fortran66.hatenablog.com/entry/2014/11/28/034535
付け加えた点は、読み込める BMP サイズを任意幅にしたところと、白地、黒地の部分に変化をつけるようにしたことです。

画像によって、経験論的パラメタを手動で調整する必要があります。絞りのｆとは逆に、数が大きいほど黒っぽい画像になります。
＞ real, parameter :: f = 1.45 !1.16      ! f empirical parameter  1.0~2.5 larger is darker

#実行例：
##プリプリちいちゃん！！
![puri2](https://user-images.githubusercontent.com/17177386/27517514-c6834278-5a08-11e7-8616-adff20247822.png)
![puri2aa](https://user-images.githubusercontent.com/17177386/27517492-7e0c5eda-5a08-11e7-9686-72452a86f9d3.png)

##Trump 大統領
![webw160901-trump-thumb-720xauto](https://user-images.githubusercontent.com/17177386/27517503-a502dcf8-5a08-11e7-9121-a8652d47ce10.jpg)
![trumpaa](https://user-images.githubusercontent.com/17177386/27517499-9a604ab0-5a08-11e7-9419-78092c084b5a.png)



#参考：
プリプリちいちゃん！！
http://pasu-chi.com/chi-chan/
