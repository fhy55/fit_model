# fit_model

## 1.データセットのシミュレーション
終了
## 2.分布の推定
### a. ヒストグラム
* 比較の文章
* どのようなbin幅が適切か
* ヒストグラムのタイトル等を整理する
### b. カーネル密度
* カーネル密度に関する関数kdensity()のデフォルトの理解
* density()の平滑化帯域幅は、bw=nrd0に設定されている。
* 望ましいのはbw=SJ　詳しくはVenablesandRipley（2002）
* デフォルトを理解した上で、デフォルト以外を試す
### c. 分位点回帰 
* OLSと結果を比較する
## 3.非線形回帰
### a. probit
* 限界効果を求める
* シミュレーションで設定したパラメーターの値を復元できるか確認
* OLSと同じ解釈ができるように、変換する
### b. Lasso
* すでに制作した以外のブートストラップサンプルに対して、Lassoを行う
* Zの定義に用いられた変数がLassoでも選ばれるか確認する
