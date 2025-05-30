---
title: 'El antropófago'
date: 2024-03-08
permalink: /posts/2024/03/antropofago/
tags:
 
---

<div style="text-align: justify;">

<div style="text-align: right;">

  <em>Select a language to translate to:</em>

 <br> <a href="https://translate.google.com/translate?sl=es&tl=en&u=https://arduinotomasi.github.io/posts/2024/03/antropofago/">English</a> |
  <a href="https://translate.google.com/translate?sl=es&tl=fr&u=https://arduinotomasi.github.io/posts/2024/03/antropofago/">Français</a> |
  <a href="https://translate.google.com/translate?sl=es&tl=it&u=https://arduinotomasi.github.io/posts/2024/03/antropofago/">Italiano</a>

</div>


<br>

<div style="background-color: rgb(221, 221, 221); padding: 14px;">
Durante un período de 12 años, entre 2007 y 2018, se desconocen las circunstancias exactas de muerte de aproximadamente <strong>7,379 ecuatorianos</strong>, lo que equivale a un promedio alarmante de 615 muertes por año, o 1.7 ecuatorianos cada día.<br>

<br>Estas cifras revelan una posible violación sistemática de los derechos humanos y una situación de impunidad generalizada en Ecuador.<br> 

<br>Cada una de estas muertes violentas sin esclarecer representa potencialmente un crimen sin resolver y una violación flagrante del derecho fundamental a la vida. La acumulación de miles de estos casos socava el Estado de Derecho y pone en tela de juicio el cumplimiento del Estado de sus obligaciones internacionales en derechos humanos.


</div><br>

</div>

<div style="text-align: justify;">


<br>En la <a href="https://arduinotomasi.github.io/posts/2024/01/Patria/">primera entrada</a> de mi trilogía que busca comprender la serie de eventos que condujeron al Ecuador a sumirse en una espiral de violencia, cuyo punto culminante fue la toma del canal público <a href="https://cnnespanol.cnn.com/2024/01/09/hombres-encapuchados-toman-las-instalaciones-de-tc-television-en-ecuador/">TC Televisión</a> <em>en vivo y en directo</em>, expuse el curioso fenómeno de que, mientras la tasa de homicidios experimentaba una caída sin precedentes, la tasa de muertes violentas de intención no determinada registraba un aumento sin igual en la historia del Ecuador desde, al menos, 1999.<br><br>

</div>

![Figure 2](/images/comparativo20242.png)<br>

<div style="text-align: justify;">

<br>En ese análisis, encontré la aparente existencia de una relación estadísticamente significativa entre esta peculiar tasa y las incautaciones de cocaína, exclusivamente durante el período comprendido entre 2010, tras la salida de la Base de Manta en septiembre de 2009, y 2018, después de la <a href="https://www.bbc.com/mundo/noticias-america-latina-49987257">ruptura política</a> entre Rafael Correa y su sucesor y exaliado, Lenin Moreno. Concretamente, encontré que cuando las incautaciones disminuyen, esta tasa aumenta, y viceversa. Asimismo, señalé que, coincidentemente, tras el cese de operaciones de la base, también se registró un drástico incremento en la tasa de tráfico de menores de 17 años.<br><br>

</div><br>

![Figure 5](/images/cocaycausaFFF.png)<br>

<div style="text-align: justify;">

<br>En mi <a href="https://arduinotomasi.github.io/posts/2024/02/Polvo/">segunda entrada</a>, analicé con mayor detalle la relación entre las incautaciones de cocaína y la tasa de muertes violentas de intención no determinada. Al hacer un <em>zoom in</em> a nivel provincial entre incautaciones (datos de nivel nacional) y tasas (a nivel de cada provincia), emergió un mapa de calor representado por el nivel de significancia estadística arrojado por la regresión lineal respectiva. Lo que comenzó como un ejercicio intelectual, repleto de números y parámetros, pareció cobrar vida propia: este mapa de calor parece coincidir notablemente con las rutas de la cocaína recientemente reveladas por la inteligencia policial; en ese sentido, el ejercicio de ir <em>hasta donde la razón y los datos nos llevaran</em> encontraba, por así decirlo, una "triangulación" de una fuente externa confiable e independiente.<br><br>

</div><br>

![Figure 5](/images/compma.png)<br>

<div style="text-align: justify;">
<br>
<br> A partir de la interpretación de este mapa de calor, construí un diseño cuasi-experimental para intentar aislar el efecto dinámico de la salida de la Base de Manta en la tasa de muertes violentas de intención no determinada. Específicamente, diseñé un estudio de eventos aplicado a un modelo de Diferencia-en-Diferencias (DiD), utilizando 252 observaciones anuales a nivel provincial entre 2007 y 2018. Esta estimación indicó que, tras el cese de operaciones de la base, las provincias clave en rutas de narcotráfico, especialmente en la costa, experimentaron <strong>un aumento del 75,48%</strong> en las tasas de muertes violentas de intención indeterminada en 2010. Este efecto se invierte con el tiempo, lo que sugiere la apertura y/o consolidación de rutas logísticas en provincias amazónicas como Napo.<br>
</div><br>


![Figure 3](/images/efectoFF2.png)<br><br>

<div style="text-align: justify;">

 <a href="https://arduinotomasi.github.io/posts/2024/02/Polvo/">En ese post</a> también realicé una serie de pruebas para verificar los supuestos clave del estimador DiD (tendencias paralelas), pruebas de falsificación asignando aleatoriamente un tratamiento placebo, y estimadores alternativos, como Newey-West con cuatro rezagos, que ajustan la heterocedasticidad y las autocorrelaciones.<br>


<br>Justo cuando estaba por dar por cerrado este tema, pensando en hacer quizás un par de pruebas más de robustez, me di cuenta de que había pasado por alto una pregunta importante. Si el enfoque cuasi-experimental verdaderamente identifica un impacto real y significativo del cierre de la base, entonces ¿de dónde vienen exactamente esas variaciones en las tasas de muertes violentas de intención no determinada? ¿Es algo que está pasando sistemáticamente en todo el país, o es acaso un efecto que solo se ve en algunas provincias en particular?<br>

<br>Dado que mi enfoque como investigador es más teórico – propiamente, me especializo en la <a href= "https://www.bbc.com/mundo/noticias/2015/02/150220_teoria_de_juegos_que_es_finde_dv">teoría de juegos</a> – que empírico, empecé por intentar definir bien mis expectativas antes de analizar cualquier dato. Mi razonamiento fue así: imaginemos que este tipo de muertes son <em>completamente</em> accidentales. Si esto fuera cierto, entonces esperaríamos que su distribución geográfica y temporal a lo largo del Ecuador sea bastante uniforme, <em> ya que no estamos hablando de un virus o una enfermedad que se propaga</em>. Si encontramos que la distribución es más o menos uniforme, entonces no podríamos atribuir directamente los resultados a la clausura de la Base de Manta en 2009. Pero, ¿qué nos dice si, al analizar las variaciones de estas tasas, vemos patrones que concentran ciertas provincias juntas, asumiendo que estas tasas reflejan simplemente accidentes?<br>

<br>Tras realizar el mapeo geográfico-temporal de las variaciones anuales de las tasas de muertes violentas de intención indeterminada, utilizando los mismos datos de mi <a href="https://arduinotomasi.github.io/posts/2024/02/Polvo/">segunda entrada</a>, me percaté de que <em>pareciera</em> como si las cifras quisieran narrar por sí mismas una historia cuya interpretación dependerá del lector. Para maximizar su experiencia de descubrimiento (si alguno), a partir de este momento minimizaré mi intervención como analista. En este ejercicio narrativo-experimental, para que el lector no se sienta abandonado, los acompañará el escritor ecuatoriano vanguardista Pablo Palacio a través de su cuento <a href= "https://ciudadseva.com/texto/el-antropofago/">'El antropófago'</a> (1927), que inspira el título de esta publicación en mi blog. La elección de este cuento fue también completamente al azar. Salvo por las descripciones y notas de las imágenes, todo el texto que sigue es de autoría de Palacio.<br>
</div>

<br>

<div style="text-align: center;">
***
</div><br>

<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">
Allí está, en la penitenciaría, asomando por entre las rejas su cabeza grande y oscilante, el antropófago.

</div><br>

</div><br>

![Figure 1](/images/2009ff.png)

<div style="text-align: justify;">
<br>

<div style="background-color: rgb(221, 221, 221); padding: 14px;">
Todos lo conocen. Las gentes caen allí como llovidas por ver al antropófago. Dicen que en estos tiempos es un fenómeno. Le tienen recelo. Van de tres en tres, por lo menos, armados de cuchillas, y cuando divisan su cabeza grande se quedan temblando, estremeciéndose al sentir el imaginario mordisco que les hace poner carne de gallina. Después le van teniendo confianza, los más valientes han llegado hasta a provocarle, introduciendo por un instante un dedo tembloroso por entre los hierros. Así repetidas veces como se hace con las aves enjauladas que dan picotazos.

</div><br>

</div><br>

![Figure 2](/images/2010ff.png)

<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Pero el antropófago se está quieto, mirando con sus ojos vacíos.<br>

<br>Algunos creen que se ha vuelto un perfecto idiota; que aquello fue solo un momento de locura.<br>

<br>Pero no les oiga; tenga mucho cuidado frente al antropófago: estará esperando un momento oportuno para saltar contra un curioso y arrebatarle la nariz de una sola dentellada.<br>

<br>Medite usted en la figura que haría si el antropófago se almorzara su nariz.
</div><br>

</div><br>

![Figure 3](/images/angostura.jpg)

<div style="text-align: center;font-size:13px;">

<a href= "https://inredh.org/doce-anos-sin-verdad-en-el-bombardeo-de-angostura/">Fundación Regional de Asesoría en Derechos Humanos, INREDH:</a><br> "Se cumplieron 13 años del bombardeo de Angostura; familiares lo recordaron con un plantón". 
</div><br>

<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

¡Ya lo veo con su aspecto de calavera!<br>

<br>¡Ya lo veo con su miserable cara de Lázaro, de sifilítico o canceroso! ¡Con el unguis asomando por entre la mucosa amoratada! ¡Con los pliegues de la boca honda, cerrados como un ángulo!
</div><br>

</div><br>

![Figure 4](/images/compff1.png)


<div style="text-align: justify;">

<br>

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Va usted a dar un magnífico espectáculo.<br>

<br>Vea que hasta los mismos carceleros, hombres siniestros, le tienen miedo.<br>

<br>La comida se la arrojan desde lejos. El antropófago se inclina, husmea, escoge la carne —que se la dan cruda— y la masca sabrosamente, lleno de placer, mientras la sanguaza le chorrea por los labios.
</div><br>

</div><br>

![Figure 5](/images/traficados.jpeg)
<div style="text-align: center;font-size:13px;">

<a href= "https://arduinotomasi.github.io/posts/2024/01/Patria/">Oficina de las Naciones Unidas contra la Droga y el Delito, UNODC:</a><br> La Trata de Personas y el Tráfico Ilícito de Migrantes
</div><br>

<div style="text-align: justify;">


<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Al principio le prescribieron dieta: legumbres y nada más que legumbres; pero había sido de ver la gresca armada. Los vigilantes creyeron que iba a romper los hierros y comérselos a toditos. ¡Y se lo merecían los muy crueles! ¡Ponérselo en la cabeza el martirizar de tal manera a un hombre habituado a servirse de viandas sabrosas! No, esto no le cabe a nadie. Carne habían de darle sin remedio, y cruda.
</div><br>

</div><br>


![Figure 5](/images/2011ff.png)

<div style="text-align: justify;">
<br>

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

¿No ha comido usted alguna vez carne cruda? ¿Por qué no ensaya?<br>

<br>Pero no, que pudiera habituarse, y esto no estaría bien. No estaría bien porque los periódicos, cuando usted menos lo piense, le van a llamar fiera, y no teniendo nada de fiera, molesta.<br>

<br>No comprenderían los pobres que el suyo sería un placer como cualquier otro; como comer la fruta en el mismo árbol, alargando los labios y mordiendo hasta que la miel corra por la barba.
</div><br>

</div><br>

![Figure 6](/images/unodc2011.png)

<div style="text-align: center;font-size:13px;">

<a href= "https://www.unodc.org/documents/data-and-analysis/WDR2011/World_Drug_Report_2011_spanish.pdf">Oficina de las Naciones Unidas contra la Droga y el Delito, UNODC:</a><br> Informe Mundial sobre 
las Drogas, 2011.
</div><br>

<br>
<div style="text-align: justify;">


<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Pero ¡qué cosas! No creáis en la sinceridad de mis disquisiciones. No quiero que nadie se forme de mí un mal concepto; de mí, una persona tan inofensiva.<br>

<br>Lo del antropófago sí es cierto, inevitablemente cierto.<br>

<br>El lunes último estuvimos a verlo los estudiantes de criminología.<br>

<br>Lo tienen encerrado en una jaula como de guardar fieras.<br>

<br>¡Y qué cara de tipo! Bien me lo he dicho siempre: no hay como los pícaros para disfrazar lo que son.
</div><br>

</div><br>

![Figure 7](/images/2012ff.png)

<div style="text-align: justify;">


<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Los estudiantes reíamos de buena gana y nos acercamos mucho para mirarlo. Creo que ni yo ni ellos lo olvidaremos. Estábamos admirados, y ¡cómo gozábamos al mismo tiempo de su aspecto casi infantil y del fracaso completo de las doctrinas de nuestro profesor!<br>

<br>— Véanlo, véanlo como parece un niño — dijo.<br>

<br>— Sí, un niño visto con una lente.<br>

<br>— Ha de tener las piernas llenas de roscas.<br>

<br>— Y deberán ponerle talco en las axilas para evitar las escaldaduras.<br>

<br>— Y lo bañarán con jabón de Reuter.<br>

<br>— Ha de vomitar blanco.<br>

<br>— Ha de oler a senos.<br>

<br>Así se burlaban los infames de aquel pobre hombre que miraba vagamente y cuya gran cabeza oscilaba como una aguja imantada.

</div><br>

</div><br>

![Figure 8](/images/compff2.png)

<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Yo le tenía compasión. La verdad, la culpa no era de él ¡Qué culpa va atener un antropófago! Menos si es hijo de un carnicero y una comadrona, como quien dice del escultor Sofronisco; y de la partera Fenareta. Eso de ser antropófago es como ser fumador, o pederasta, o sabio.<br>

<br>Pero los jueces le van a condenar irremediablemente, sin hacerse estas consideraciones. Van a castigar una inclinación naturalísima: esto rebela. Yo no quiero que se proceda de ninguna manera en mengua de la justicia. Por esto quiero dejar constancia, en unas pocas líneas, de mi adhesión al antropófago. Y creo que sostengo una causa justa. Me refiero a la irresponsabilidad que existe de parte de un ciudadano cualquiera, al dar satisfacción a un deseo que desequilibra atormentadoramente su organismo.<br>

<br>Hay que olvidar por completo toda palabra hiriente que yo haya escrito en contra de ese pobre irresponsable. Yo, arrepentido, le pido perdón.<br>

</div><br>

</div><br>

![Figure 9](/images/2013ff.png)

<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Sí, sí, creo sinceramente que el antropófago está en lo justo; que no hay razón para que los jueces, representantes de la vindicta pública…<br>

<br>Pero qué trance tan duro… Bueno… lo que voy a hacer es referir con sencillez lo ocurrido…<br>

<br>No quiero que ningún malintencionado diga después que soy yo pariente de mi defendido, como ya me lo dijo un comisario a propósito de aquel asunto de Octavio Ramírez.<br>

<br>Así sucedió la cosa, con antecedentes y todo:

</div><br>

</div><br>

![Figure 6](/images/unodc2013.png)

<div style="text-align: center;font-size:13px;">

<a href= "https://www.unodc.org/doc/wdr2013/World_Drug_Report_2013_Spanish.pdf">Oficina de las Naciones Unidas contra la Droga y el Delito, UNODC:</a><br> Informe Mundial sobre 
las Drogas, 2013.
</div><br>

<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

En un pequeño pueblo del sur, hace más o menos treinta años, contrajeron matrimonio dos conocidos habitantes de la localidad: Nicanor Tiberio, dado al oficio de matarife, y Dolores Orellana, comadrona y abacera.<br>

<br>A los once meses justos de casados les nació un muchacho, Nico, el pequeño Nico, que después se hizo grande y ha dado tanto que hacer.<br>

<br>La señora de Tiberio tenía razones indiscutibles para creer que el niño era oncemesino, cosa rara y de peligros. De peligros porque quien se nutre con tanto tiempo de sustancias humanas es lógico que sienta más tarde la necesidad de ellas.<br>

<br>Yo desearía que los lectores fijen bien su atención en este detalle, que es a mi ver justificativo para Nico Tiberio y para mí, que he tomado cartas en el asunto.<br>

</div><br>

</div><br>

![Figure 6](/images/compff3.png)

<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Bien. La primera lucha que suscitó el chico en el seno del matrimonio fue a los cinco años, cuando ya vagabundeaba y comenzó a tomársele en serio. Era a propósito de la profesión. Una divergencia tan vulgar y usual entre los padres, que casi, al parecer, no vale la pena darle ningún valor. Sin embargo, para mí lo tiene.<br>

<br>Nicanor quería que el muchacho fuera carnicero, como él. Dolores opinaba que debía seguir una carrera honrosa, la medicina. Decía que Nico era inteligente y que no había que desperdiciarlo. Alegaba con lo de las aspiraciones — las mujeres son especialistas en lo de las aspiraciones.<br>

<br>Discutieron el asunto tan acremente y tan largo que a los diez años no lo resolvían todavía. El uno: que carnicero ha de ser; la otra: que ha de llegar a médico. A los diez años Nico tenía el mismo aspecto de un niño; aspecto que creo olvidé de describir. Tenía el pobre muchacho una carne tan suave que le daba ternura a su madre; carne de pan mojado en leche, como que había pasado tiempo curtiéndose en las entrañas de Dolores.<br>

<br>Pero pasa que el infeliz había tomádole serias aficiones a la carne. Tan serias que ya no hubo que discutir: era un excelente carnicero. Vendía y despostaba que era de admirarlo.<br>

</div><br>

</div><br>

![Figure 6](/images/2014ff.png)

<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Dolores, despechada, murió el 15 de mayo del 1909 (¿Será también este un dato esencial?). Tiberio, Nicanor Tiberio, creyó conveniente emborracharse seis días seguidos y el séptimo, que en rigor era de descanso, descansó eternamente. (Uf, esta va resultando tragedia de cepa).<br>

<br>Tenemos, pues, al pequeño Nico en absoluta libertad para vivir a su manera solo a la edad de diez años.<br>

<br>Aquí hay un lago en la vida de nuestro hombre. Por más que he hecho, no he podido recoger los datos suficientes para reconstruirla. Parece, sin embargo, que no sucedió en ella circunstancia alguna capaz de llamar la atención de sus compatriotas.<br>

<br>Una que otra aventurilla y nada más.<br>

<br>Lo que se sabe a punto fijo es que se casó, a los veinticinco, con una muchacha de regulares proporciones y medio simpática. Vivieron más o menos bien. A los dos años les nació un hijo, Nico, de nuevo Nico.<br>

<br>De este niño se dice que creció tanto en saber y en virtudes, que a los tres años, por esta época leía, escribía, y era tipo correcto: uno de esos niños seriotes y pálidos en cuyas caras aparece congelado el espanto.<br>

<br>La señora de Nico Tiberio (del padre, no vaya a creerse que del niño) le había echado ya el ojo a la abogacía, carrera magnífica para el chiquitín. Y algunas veces había intentado decírselo a su marido. Pero este no daba oídos, refunfuñando: ¡Esas mujeres que andan siempre metidas en lo que no les importa!<br>

<br>Bueno, esto no le interesa a Ud., sigamos con la historia:

</div><br>

</div><br>

![Figure 6](/images/2015ff.png)


<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

La noche del 23 de marzo, Nico Tiberio, que vino a establecerse en la capital tres años atrás con la mujer y el pequeño — dato que he olvidado de referir a su tiempo —, se quedó hasta bien tarde en un figón de San Roque, bebiendo y charlando.<br>

<br>Estaba con Daniel Cruz y Juan Albán, personas bastante conocidas que prestaron, con oportunidad, sus declaraciones ante el juez competente. Según ellos, el tantas veces nombrado Nico Tiberio no dio manifestaciones extraordinarias que pudieran hacer luz en su decisión. Se habló de mujeres y de platos sabrosos. Se jugó un poco a los dados. Cerca de la una de la mañana, cada cual la tomó por su lado.<br>

<br>(Hasta aquí las declaraciones de los amigos del criminal. Después viene su confesión, hecha impúdicamente para el público).<br>

<br>Al encontrarse solo, sin saber cómo ni por qué, un penetrante olor a carne fresca empezó a obsesionarlo. El alcohol le calentaba el cuerpo y el recuerdo de la conversación le producía abundante saliveo. A pesar de lo primero, estaba en sus cabales.<br>

<br>Según él, no llegó a precisar sus sensaciones. Sin embargo, aparece bien claro lo siguiente:

</div><br>

</div><br>

![Figure 6](/images/compff4.png)


<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Al principio le atacó un irresistible deseo de mujer. Después le dieron ganas de comer algo bien sazonado; pero duro, cosa de dar trabajo a las mandíbulas. Luego le agitaron temblores sádicos: pensaba en una rabiosa cópula, entre lamentos, sangre y heridas abiertas a cuchilladas.<br>

<br>Se me figura que andaría tambaleando, congestionado.<br>

<br>A un tipo que encontró en el camino casi le asalta a puñetazos, sin haber motivo.<br>

<br>A su casa llegó furioso. Abrió la puerta de una patada. Su pobre mujercita despertó con sobresalto y se sentó en la cama. Después de encender la luz se quedó mirándolo temblorosa, como presintiendo algo en sus ojos colorados y saltones.<br>

<br>Extrañada, le preguntó:<br>

<br>— ¿Pero qué te pasa, hombre?<br>

<br>Y él, mucho más borracho de lo que debía estar, gritó:<br>

<br>— Nada, animal; ¿a ti qué te importa?; ¡a echarse!<br>

<br>Mas en vez de hacerlo, se levantó del lecho y fue a pararse en medio de la pieza. ¿Quién sabía qué le irían a mentir a ese bruto?

</div><br>

</div><br>

![Figure 6](/images/2016ff.png)


<div style="text-align: justify;">

<div style="background-color: rgb(221, 221, 221); padding: 14px;">

La señora de Nico Tiberio, Natalia, es morena y delgada.<br>

<br>Salido del amplio escote de la camisa de dormir, le colgaba un seno duro y grande. Tiberio, abrazándola furiosamente, se lo mordió con fuerza. Natalia lanzó un grito.<br>

<br>Nico Tiberio, pasándose la lengua por los labios, advirtió que nunca había probado manjar tan sabroso.

</div><br><br>

</div>

![Figure 6](/images/compff5.png)


<div style="text-align: justify;">



<div style="background-color: rgb(221, 221, 221); padding: 14px;">

¡Pero no haber reparado nunca en eso! ¡Qué estúpido!<br>

<br>¡Tenía que dejar a sus amigotes con la boca abierta!<br>

<br>Estaba como loco, sin saber lo que le pasaba y con un justificable deseo de seguir mordiendo.<br>

<br>Por fortuna suya oyó los lamentos del chiquitín, de su hijo, que se frotaba los ojos con las manos.<br>

</div><br>

</div><br>




![Figure 6](/images/2017ff.png)


<div style="text-align: justify;">



<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Se abalanzó gozoso sobre él; lo levantó en sus brazos, y abriendo mucho la boca, empezó a morderle la cara, arrancándole regulares trozos a cada dentellada, riendo, bufando, entusiasmándose cada vez más.<br>

<br>El niño se esquivaba de él que se lo comía por el lado más cercano, sin dignarse escoger.<br>

<br>Los cartílagos sonaban dulcemente entre los molares del padre. Se chupaba los dientes y lamía los labios.<br>

<br>¡El placer que debió sentir Nico Tiberio!

</div><br>

</div><br>




![Figure 6](/images/compff6.png)


<div style="text-align: justify;">



<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Y como no hay en la vida cosa cabal, vinieron los vecinos a arrancarle de su abstraído entretenimiento. Le dieron de garrotazos, con una crueldad sin límites, le ataron, cuando le vieron tendido y sin conocimiento; le entregaron a la policía…<br>

<br>¡Ahora se vengarán de él!<br>

</div><br>

</div><br>



![Figure 6](/images/2018ff.png)


<div style="text-align: justify;">



<div style="background-color: rgb(221, 221, 221); padding: 14px;">

Pero Tiberio (hijo), se quedó sin nariz, sin orejas, sin una ceja, sin una mejilla.<br>

<br>Así, con su sangriento y descarbado aspecto, parecía llevar en la cara todas las ulceraciones de un hospital.<br>

<br>Si yo creyera a los imbéciles tendría que decir: Tiberio (padre) es como quien se come lo que crea. 
</div><br>

</div>



![Figure 5](/images/traficados.jpeg)
<div style="text-align: center;font-size:13px;">

<a href= "https://arduinotomasi.github.io/posts/2024/01/Patria/">Oficina de las Naciones Unidas contra la Droga y el Delito, UNODC:</a><br> La Trata de Personas y el Tráfico Ilícito de Migrantes
</div><br><br><br>


<hr>
<hr>
<br>
<div style="text-align: center;" >
<strong>Replicación</strong>

</div>
<br>

<div style="text-align: justify;" >

<strong>A.</strong> La base de datos para el mapeo geográfico-temporal la pueden encontrar haciendo click <a href="https://www.dropbox.com/scl/fi/oxwkex7lt8l3r8exnswvp/Var.csv?rlkey=p59j95cjsb7l3dkbytc4pc7cg&dl=0">aquí</a>.<br> 

<br><strong>B.</strong> La variable relevante se denomina "Var" y cuantifica la variación porcentual del logaritmo natural de la tasa. En concreto, para eludir cualquier división por cero durante la transformación, se aplica la fórmula $\ln(2+\text{TasaViolentasIndetermidas})$ a los datos de la tasa para asegurar comparabilidad entre provincias.<br>

<br><strong>C.</strong> En este <a href="https://gadm.org/download_country.html">link</a> pueden descargar el <em> Shapefile </em> para realizar el mapeo, con el archivo pertinente denominado 'gadm41_ECU_1.shp'. Luego, pueden aplicar el comando en R que se muestra a continuación.

</div>
<br>

<div style="display: flex; justify-content: center;">

<pre style="background-color: #f4f4f4; color: #333; padding: 20px; border: 2px solid #000; box-shadow: 0 4px 6px rgba(0,0,0,0.1); font-family: 'Courier New', Courier, monospace; line-height: 1.5; overflow-x: auto; font-size: 12px;">
<code>
# Cargar las bibliotecas
library(ggplot2)
library(tidyverse) 
library(sf)        
library(readr)     

# Cargar los datos 
data <- read_csv('/Var.csv')
provinces <- st_read('/gadm41_ECU_1.shp')

# Ejemplo: Filtrar los datos para el año 2010
filtered_data <- data %>% filter(Year == 2010)

# Combinar los datos filtrados con el archivo de forma de las provincias
final_data <- provinces %>% left_join(filtered_data, by = "CC_1")

# Calcular el centroide de las geometrías para mejorar la visualización
final_data$geometry_centroid <- st_centroid(final_data$geometry)

# Definir los límites mínimos y máximos de la variable "Var"
min_var <- max(0, min(final_data$Var, na.rm = TRUE)) # Mínimo valor de "Var", pero no menos que 0
max_var <- 50 # Máximo 

# Crear el mapa para el ejemplo del año 2010
ggplot(final_data) +
  geom_sf(aes(fill = Var), color = "black") + 
  scale_fill_gradient(
    low = "green", 
    high = "black", 
    name = " ", # Nombre de la barra de colores
    limits = c(min_var, max_var), 
    breaks = c(min_var, max_var),
    labels = c(paste0(round(min_var, 2), "%"), paste0(round(max_var, 2), "%")), 
    na.value = "black", 
    oob = scales::squish 
  ) +
  geom_sf_text(aes(label = Provincia), size = 2.7, color = "black", check_overlap = TRUE) + 
  coord_sf(xlim = c(-81.5, -75), ylim = NULL) + 
  labs( # Etiquetas del gráfico
    title = "Tasa de Muertes Violentas de Intención no Determinada",
    subtitle = "Aumentos en variación porcentual anual. Año 2010",
    caption = " ",
    fill = " "
  ) +
  theme_minimal() + 
  theme(
    plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 5, b = 0), face = "plain"),
    plot.title = element_text(hjust = 0.5, size = 15, margin = margin(t = 0, b = 8), face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(t = 0, b = -8), face = "plain"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.margin = margin(t = -15, b = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.margin = margin(t = 5, r = -5, b = 5, l = -5, unit = "mm")
  )



</code></pre>
</div>
