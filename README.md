# MQ-UFMG 2018 - Web Scraping: Captura e análise de dados da web com R

Bem vind@s à página do curso "Captura, sistematização e análise de dados da web com R". Nesta página você encontrará os tutoriais, dados, e demais materiais que utilizaremos no curso, organizados por dia.

## Informações Básicas

Professores: 
Denisson Silva 
Rogério Jerônimo Barbosa

Data: 23 de Julho a 27 de Agosto de 2018
Horário: 17h00 às 20h00

## Aulas

### Dia 1 - 31/07/2017 - O básico da captura de páginas com R - páginas com tabelas

No primeiro dia do curso faremos uma breve apresentação do curso, da linguagem R e produziremos 2 exemplos simples de captura de dados na internet com páginas que contêm tabelas:

1- Em conjunto, faremos a raspagem dos dados sobre servidores públicos federais no portal da transparência.

2- A  seguir, faremos individualmente ou em pequenos grupos o [Tutorial 1](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial1.Rmd).

### Dia 2 - 01/08/2017 - O básico da captura de páginas com R - portais de notícias

Uma vez que compreendemos a lógica da captura de várias páginas simples com tabela, vamos aprender um pouco sobre HTML e XML para extrair conteúdos quaisquer (e não apenas tabelas) de páginas de internet. 
Vamos começar a aula com o [Tutorial 2](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial2.Rmd). Neste tutorial, vamos aprender a montar um banco de dados com os títulos e links de notícias resultantes de uma única página de busca em um portal de notícias.

A seguir, vamos aproveitar o embalo dos Tutoriais 1 e 2 e aprender a construir um banco de dados com os títulos e links de notícias resultantes de todas as página de busca em um portal de notícias. Faremos isso no [Tutorial 3](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial3.Rmd).

Finalmente, vamos contruir um banco de dados de notícias -- título, link, data e hora, e conteúdo no [Tutorial 4](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial4.Rmd).

É possível que não terminemos os 3 tutoriais nesta aula. Não se preocupem. Continuaremos na aula seguinte.

### Dia 3 - 02/08/2017 - Mineração de texto com R - uma introdução breve

Hoje vamos aprender sobre algumas maneiras de trabalharmos com texto como dado no R e conhecer 3 pacotes novos: _stringr_, _tm_ e _tidytext_.

Originalmente, este material fez parte de um programa completo de programação com R. Minha expectativa é que aqueles de vocês que fizeram o curso do MQ de Introdução ao R tenham menos problemas com aspectos da linguagem que ainda não apareceram em nosso curso. Para os demais, vou indicar materiais complementares que ajudam a compreender os tutoriais de hoje.

Como há vários novos aspectos da linguagem, não exitem em nos chamar para compreender aquilo que é novo ou estranho.

Vamos começar com o [Tutorial 5](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial5.Rmd), que introduz as funções do pacote _stringr_ e captura os dados com os quais vamos trabalhar.

A seguir, vamos aprender sobre "corpus" com o pacote _tm_. Vá para o [Tutorial 6](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial6.Rmd).

Terminaremos com a abordagem "tidy" para texto (pacote _tidytext_), no [Tutorial 7](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial7.Rmd).

### Dias 4 e 5 - 03/08/2017 e 04/08/2017 - Tópicos de captura de dados na internet

Nos dois últimos dias do curso vamos trabalhar com tópicos avulsos e ligeiramente mais avançados de captura de dados na internet e/ou em documentos em formato .pdf. Os tópicos são independentes entre si e você pode gerenciar o seu tempo para terminar tutoriais anteriores, estudar material de apoio e avançar nos tutoriais de cada tópico. Minha única recomendação é que você não deixe de fazer o tutorial sobre formulários, pois trata-se de uma "classe" de problemas bastante comum para captura de dados.

- [Formulários Web](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial8.Rmd) - Enviando dados em formulários na web com o pacote _rvest_.
- [Twitter](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial9.Rmd) - Pacote _twitteR_ para acessar a API do twitter.
- [PDF](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial10.Rmd) - Transformando arquivos em formato .pdf em texto com o pacote _pdftools_.
- [Facebook](https://github.com/leobarone/mq_ufmg_17/blob/master/mq_ufmg_2017_tutorial11.Rmd) - Pacote _Rfacebook_ para acessar a API do facebook (bastante limitada).

## Extras e materiais de apoio

Os materiais de apoio são os tutoriais do curso "FLS 6397 - Introdução à Programação e Ferramentas Computacionais para as Ciências Sociais" que ministrei no primeiro semestre de 2017 do Departamento de Ciência Política da Universidade de São Paulo. Você pode procurar visitar a página do curso completo [aqui](https://github.com/leobarone/FLS6397).

### R Básico

- Para quem precisa estudar mais sobre data frames e vetores, clique [aqui](https://github.com/leobarone/FLS6397/blob/master/tutorials/tutorial1.Rmd)

- Para aprender sobre as funcionalidades básicas do R, veja este [tutorial](https://github.com/leobarone/FLS6397/blob/master/tutorials/tutorial2.Rmd)

- Finalmente, este terceiro [tutorial](https://github.com/leobarone/FLS6397/blob/master/tutorials/tutorial3.Rmd) trata de operadores relacionais e lógicos, cláusulas condicionais, loops e funções.

### Manipulação de dados no R

- Se você quer ver um pouco mais sobre abertura de dados no R, veja este [tutorial](https://github.com/leobarone/FLS6397/blob/master/tutorials/tutorial4.Rmd)

- Para aprender sobre manipulação de dados com a gramática básica do R, leia este [aqui](https://github.com/leobarone/FLS6397/blob/master/tutorials/tutorial5.Rmd)

- Finalmente, se quiser aprender sobre o pacote _dplyr_ faça os sobre o [básico do _dplyr_](https://github.com/leobarone/FLS6397/blob/master/tutorials/tutorial6.Rmd) e [bases de dados relacionais com dplyr](https://github.com/leobarone/FLS6397/blob/master/tutorials/tutorial7.Rmd) 

### R for Data Science

Uma das melhores referências para estudar ciência de dados com R é o livro [R for Data Science](http://r4ds.had.co.nz/) de Grolemund e Wickham. O livro é online gratuito. Sugiro utilizá-lo como material de apoio durante o curso e é a leitura que recomendo após finalizarmos.

### Captcha e Curso-R

Uma fonte bastante interessante sobre R em português é o [Curso-R](http://curso-r.com), desenvolvido pelo pessoal do IME-USP. Vale a pena acompanhar.

Recentemente, há uma sequência de posts sobre como quebrar captcha com R no blog, [aqui](http://curso-r.com/tags/captcha/). Sugiro a leitura para quem se deparar com captchas em uma captura de dados.

## Atividade de avaliação

A avaliação do curso é composta de participação nas atividades em sala de aula e de duas atividades a serem entregues até (DATA A DEFINIR). Leia a seguir as instruções das atividades.

Em cada uma das atividades você deve entregar um script que cumpre a tarefa. Os script deve conter comentários sobre cada passo.

Não se preocupe em um script "que funcione". Entregue o melhor que puder e aponte em comentários dificuldades, dúvidas e problemas encontrados.

### 1 - Resumos e nuvem de palavras de um periódico no Scielo

* Escolha uma revista qualquer na [lista de periódicos brasileiros no Scielo](http://www.scielo.br/scielo.php?script=sci_alphabetic&lng=pt&nrm=iso).

* Selecionado o periódico, clique na aba "todos" para acessar a tabela com todos os números da revista.

* Capture os URLs para todos os números da revista. Dica: você não precisa de um "for loop" nesta etapa. Guarde os links em um vetor ou em um data frame.

* A seguir, capture (em loop!) as páginas de todos os números e extraia delas os links para o resumo em português. Se algum artigo não tiver resumo em português, você pode ignorá-lo. Dica: como há muitos links na página, vale a pena capturar mais links que o desejado e selecionar aqueles que se referem apenas aos resumos em português. Os links dos resumos se distinguem pela string "script=sci_abstrac" no endereço em URL. Os links para resumos e textos em português terminam sempre com "tlng=pt". Com o pacote _stringr_ você pode excluir os links que não interessam.

Exemplo: http://www.scielo.br/scielo.php?script=sci_abstract&pid=S0103-636X2017000100019&lng=pt&nrm=iso&tlng=pt

* Tendo capturado os links para cada artigo, captura (em loop novamente!) os textos de cada um dos resumos em português. Dica: você pode capturar todas as tags "p" que estão juntas ao resumo e selecionar depois apenas o resumo pela posição (ou pode deixar tudo junto se for difícil separar).

* Faça uma nuvem de palavras com o conteúdo dos resumos capturado.

### Atividade 2 - Formulário ou twitter

Há três opções para esta atividade. Você precisa entregar apenas uma opção.

* Opção 1 - Capturar dados via API do Twitter e construir uma visualização interessante com os dados capturados. Não é preciso seguir nenhuma instrução específica e você pode utiliar sua criatividade. Utilize o tutorial sobre captura dados no twitter como apoio. Obs: não se preocupe em enviar as "keys" de sua conta de desenvolvedor, pois posso inserir as minhas ao corrigir sua atividade. 

* Opção 2 - Elaborar um código (ou função, caso saiba criar funções no R) para enviar uma requisição a um formulário na web e capturar seu resultado. Tente escolher um formulário com poucos parâmetros e priorize formulários que você consegue entender a formulários que te interessam para pesquisa ou trabalho. Novamente, as instruções são livres e você pode usar sua criatividade.

* Opção 3 - Fazer download de uma quantidade grande de documentos em .pdf que não precisem de OCR (ou seja, cujo texto é reconhecido como texto e não como imagem) e transformá-lo em um corpus ou banco de dados. Novamente, instruções livres.

## Créditos e agradecimentos

Rogério Barbosa e Alexia Aslan contribuíram em versões anteriores deste curso ou deste material. Agradecimentos a Guilherme Andrade (2014 e 2015) e a Lucas Gelape (2017), que contribuíram enormemente como assistentes nas diferentes edições deste curso.
