# app.R - نسخه بهینه شده
library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(gridExtra)
library(moments)
library(car)
library(MASS)
library(broom)
library(survival)
library(survminer)
library(tidyr)
library(memoise) # اضافه کردن caching برای محاسبات سنگین

# تعریف توابع سنگین برای caching
calculate_normal_distribution <- function(mean, sd, n) {
  # شبیه‌سازی سنگین توزیع نرمال
  Sys.sleep(0.5) # شبیه‌سازی محاسبه سنگین
  set.seed(123)
  data <- rnorm(n, mean, sd)
  return(list(
    data = data,
    mean = mean(data),
    sd = sd(data),
    summary = summary(data)
  ))
}

calculate_ttest <- function(group1, group2) {
  # شبیه‌سازی آزمون t سنگین
  Sys.sleep(0.3)
  result <- t.test(group1, group2)
  return(result)
}

calculate_correlation <- function(x, y) {
  # شبیه‌سازی محاسبه همبستگی
  Sys.sleep(0.2)
  return(list(
    pearson = cor.test(x, y, method = "pearson"),
    spearman = cor.test(x, y, method = "spearman")
  ))
}

# ایجاد نسخه cached از توابع
mem_normal_dist <- memoise(calculate_normal_distribution)
mem_ttest <- memoise(calculate_ttest)
mem_correlation <- memoise(calculate_correlation)

# استفاده از reactiveVal برای مدیریت state
data_reactive <- reactiveVal()

# تنظیمات اولیه برای بهینه‌سازی
options(shiny.maxRequestSize = 10*1024^2) # محدود کردن حجم آپلود
options(shiny.reactlog = FALSE)
options(warn = -1) # غیرفعال کردن هشدارها

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      /* استایل‌های پایه و مشترک */
      .rtl-text {
        text-align: right;
        direction: rtl;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      .rtl-list {
        text-align: right;
        direction: rtl;
      }
      .farsi-font {
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      .table-rtl {
        text-align: right;
        direction: rtl;
      }
      .center-content {
        text-align: center;
      }
      .rtl-input {
        text-align: right;
        direction: rtl;
      }
      .sticky-header {
        position: sticky;
        top: 0;
        background-color: white;
        z-index: 100;
        padding: 10px;
        border-bottom: 2px solid #007bff;
        margin-bottom: 15px;
      }
      
      /* استایل‌های boxهای رنگی */
      .highlight-box {
        background-color: #f8f9fa;
        border-right: 4px solid #007bff;
        padding: 15px;
        margin: 10px 0;
        border-radius: 5px;
      }
      .warning-box {
        background-color: #fff3cd;
        border-right: 4px solid #ffc107;
        padding: 15px;
        margin: 10px 0;
        border-radius: 5px;
      }
      .success-box {
        background-color: #d1edff;
        border-right: 4px solid #0dcaf0;
        padding: 15px;
        margin: 10px 0;
        border-radius: 5px;
      }
      .info-box {
        background-color: #f8f9fa;
        border-right: 4px solid #17a2b8;
        padding: 12px;
        margin: 8px 0;
        border-radius: 5px;
        font-size: 14px;
      }
      
      /* استایل‌های پیشرفته و زیبا */
      .sidebar-custom {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 10px;
        padding: 15px;
        margin: 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .nav-custom .nav-link {
        color: #333 !important;
        border-radius: 8px;
        margin: 5px 0;
        transition: all 0.3s ease;
        border-right: 3px solid transparent;
        text-align: right;
        direction: rtl;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      
      .nav-custom .nav-link:hover {
        background-color: #e3f2fd;
        border-right: 3px solid #007bff;
        transform: translateX(-5px);
      }
      
      .nav-custom .nav-link.active {
        background-color: #007bff !important;
        color: white !important;
        border-right: 3px solid #0056b3;
        font-weight: bold;
      }
      
      .section-header {
        background: linear-gradient(45deg, #007bff, #0056b3);
        color: white;
        padding: 10px 15px;
        border-radius: 8px;
        margin: 15px 0;
        text-align: center;
        font-weight: bold;
        font-size: 16px;
      }
      
      .logo-container {
        text-align: center;
        padding: 20px 10px;
        background: white;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .logo-text {
        font-size: 24px;
        font-weight: bold;
        color: #007bff;
        margin-bottom: 5px;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      
      .logo-subtext {
        font-size: 14px;
        color: #6c757d;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      
      .nav-icon {
        margin-left: 8px;
        font-size: 16px;
      }
      
      .header-title {
        font-size: 28px;
        font-weight: bold;
        color: #007bff;
        text-align: right;
        padding: 15px 0;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      
      /* استایل‌های شماره فصل‌ها و بخش‌ها */
      .chapter-number {
        display: inline-block;
        width: 30px;
        height: 30px;
        background: linear-gradient(45deg, #007bff, #0056b3);
        color: white;
        border-radius: 50%;
        text-align: center;
        line-height: 30px;
        margin-left: 10px;
        font-weight: bold;
        font-size: 14px;
      }
      
      .chapter-title {
        font-size: 22px;
        font-weight: bold;
        color: #2c3e50;
        border-right: 4px solid #007bff;
        padding-right: 15px;
        margin: 20px 0;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      
      .section-number {
        display: inline-block;
        width: 25px;
        height: 25px;
        background: #28a745;
        color: white;
        border-radius: 50%;
        text-align: center;
        line-height: 25px;
        margin-left: 8px;
        font-weight: bold;
        font-size: 12px;
      }
      
      .subsection-number {
        display: inline-block;
        width: 20px;
        height: 20px;
        background: #6c757d;
        color: white;
        border-radius: 50%;
        text-align: center;
        line-height: 20px;
        margin-left: 6px;
        font-weight: bold;
        font-size: 10px;
      }

      .section-title {
        font-size: 20px;
        font-weight: bold;
        color: #2c3e50;
        border-right: 3px solid #28a745;
        padding-right: 12px;
        margin: 15px 0;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      
      .subsection-title {
        font-size: 18px;
        font-weight: bold;
        color: #2c3e50;
        border-right: 2px solid #6c757d;
        padding-right: 10px;
        margin: 12px 0;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      
      .subsubsection-title {
        font-size: 16px;
        font-weight: bold;
        color: #2c3e50;
        margin: 10px 0;
        font-family: 'Tahoma', 'Arial', sans-serif;
      }
      
      /* استایل‌های جدید برای فصل ۱۶ */
      .checklist-item {
        padding: 10px;
        margin: 5px 0;
        border-right: 3px solid #28a745;
        background-color: #f8fff9;
        border-radius: 5px;
      }
      .checklist-item.checked {
        background-color: #e8f5e8;
        text-decoration: line-through;
      }
      .tool-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 15px;
        border-radius: 10px;
        margin: 10px 0;
      }
      .conversion-tool {
        background-color: #e3f2fd;
        border: 2px solid #2196f3;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
      }
      .calculator-box {
        background: linear-gradient(45deg, #4CAF50, #45a049);
        color: white;
        padding: 15px;
        border-radius: 8px;
        margin: 10px 0;
      }
      .resource-box {
        background-color: #fff3cd;
        border: 2px solid #ffc107;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
      }
      
      /* استایل‌های برای جداول */
      .custom-table {
        width: 100%;
        border-collapse: collapse;
        margin: 15px 0;
      }
      .custom-table th {
        background: linear-gradient(45deg, #007bff, #0056b3);
        color: white;
        padding: 12px;
        text-align: center;
      }
      .custom-table td {
        padding: 10px;
        border: 1px solid #dee2e6;
        text-align: center;
      }
      .custom-table tr:nth-child(even) {
        background-color: #f8f9fa;
      }
      .custom-table tr:hover {
        background-color: #e3f2fd;
      }
      
      /* استایل‌های برای دکمه‌ها */
      .btn-custom {
        background: linear-gradient(45deg, #007bff, #0056b3);
        color: white;
        border: none;
        padding: 10px 20px;
        border-radius: 5px;
        font-weight: bold;
        transition: all 0.3s ease;
      }
      .btn-custom:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
      .btn-success-custom {
        background: linear-gradient(45deg, #28a745, #20c997);
        color: white;
      }
      .btn-warning-custom {
        background: linear-gradient(45deg, #ffc107, #fd7e14);
        color: white;
      }
      
      /* استایل‌های برای نمودارها */
      .plot-container {
        background: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin: 15px 0;
      }
      
      /* استایل‌های برای فهرست */
      .custom-list {
        list-style-type: none;
        padding-right: 0;
      }
      .custom-list li {
        padding: 8px 0;
        border-bottom: 1px solid #e9ecef;
      }
      .custom-list li:before {
        content: '✓';
        color: #28a745;
        font-weight: bold;
        margin-left: 10px;
      }
      
      /* استایل‌های برای فرم‌ها */
      .form-control-custom {
        border: 2px solid #007bff;
        border-radius: 5px;
        padding: 10px;
        text-align: right;
      }
      .form-control-custom:focus {
        border-color: #0056b3;
        box-shadow: 0 0 5px rgba(0,123,255,0.5);
      }
      
      /* استایل‌های ریسپانسیو */
      @media (max-width: 768px) {
        .header-title {
          font-size: 20px;
        }
        .chapter-title {
          font-size: 18px;
        }
        .section-title {
          font-size: 16px;
        }
      }
    "))
  ),
  
  div(class = "sticky-header",
      fluidRow(
        column(2,
               div(class = "logo-container",
                   div(class = "logo-text", "📊 آمارپزشکی")
               )
        ),
        column(10,
               div(class = "header-title", 
                   "آموزش آمار مقدماتی برای دانشجویان پزشکی - ویرایش ۱.۰")
        )
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebar-custom",
      
      div(class = "nav-custom",
          h4("🎯 فهرست سرفصل‌ها", style = "color: white; text-align: center;"),
          
          div(class = "section-header", "📚 مباحث آمار"),
          navlistPanel(
            id = "tabs",
            widths = c(12, 12),
            "",
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۱"), "مقدمه و اهمیت آمار"), 
                     value = "intro",
                     icon = icon("home")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۲"), "انواع متغیرها و نمونه‌گیری"), 
                     value = "variables",
                     icon = icon("chart-bar")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۳"), "آمار توصیفی"), 
                     value = "descriptive",
                     icon = icon("chart-line")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۴"), "توزیع‌های آماری"), 
                     value = "distributions",
                     icon = icon("bell")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۵"), "آزمون فرض و فاصله اطمینان"), 
                     value = "tests_ci",
                     icon = icon("check-circle")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۶"), "آزمون‌های یک و دو گروه"), 
                     value = "statistical_tests",
                     icon = icon("vial")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۷"), "آزمون‌های چند گروه مستقل"), 
                     value = "multiple_groups",
                     icon = icon("users")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۸"), "آزمون‌های متغیرهای کیفی"), 
                     value = "categorical_tests",
                     icon = icon("list-alt")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۹"), "آنالیز کوواریانس (ANCOVA)"), 
                     value = "ancova",
                     icon = icon("sliders-h")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۱۰"), "مقایسه گروه‌های وابسته"), 
                     value = "repeated_measures",
                     icon = icon("sync")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۱۱"), "همبستگی"), 
                     value = "correlation",
                     icon = icon("link")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۱۲"), "رگرسیون خطی"), 
                     value = "linear_regression",
                     icon = icon("line-chart")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۱۳"), "رگرسیون لجستیک"), 
                     value = "logistic_regression",
                     icon = icon("project-diagram")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۱۴"), "رگرسیون شمارشی"), 
                     value = "count_regression",
                     icon = icon("calculator")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۱۵"), "تحلیل بقا"), 
                     value = "survival_analysis",
                     icon = icon("heartbeat")),
            tabPanel(span(class = "chapter-item", span(class = "chapter-number", "۱۶"), "نکات کاربردی"), 
                     value = "tips",
                     icon = icon("lightbulb"))
          )
      )
    ),
    
    mainPanel(
      width = 9,
      uiOutput("main_content")
    )
  )
)

server <- function(input, output, session) {
  output$main_content <- renderUI({
    selected_tab <- input$tabs
    
    switch(selected_tab,
           
           "intro" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۱: مقدمه و اهمیت آمار در پزشکی"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱.۱"), "مقدمه آمار"
                 ),
                 p("آمار (Statistics) علم جمع‌آوری، تحلیل، تفسیر و ارائه داده‌ها است. در پزشکی مدرن، هیچ تصمیمی بدون پشتوانه آماری گرفته نمی‌شود."),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱.۱.۱"), "تعریف آمار"
                 ),
                 p("آمار را می‌توان به دو بخش اصلی تقسیم کرد:"),
                 tags$ul(
                   tags$li(tags$b("آمار توصیفی (Descriptive Statistics):"), "خلاصه‌سازی و نمایش داده‌ها با استفاده از نمودارها و شاخص‌ها."),
                   tags$li(tags$b("آمار استنباطی (Inferential Statistics):"), "استفاده از نمونه‌ها برای نتیجه‌گیری درباره جامعه بزرگتر.")
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱.۲"), "کاربردهای آمار در پزشکی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱.۲.۱"), "کاربردهای بالینی"
                 ),
                 tags$ul(
                   tags$li("تعیین اثربخشی داروهای جدید (کارآزمایی‌های بالینی)"),
                   tags$li("بررسی شیوع بیماری‌ها در جمعیت‌های مختلف"),
                   tags$li("شناسایی عوامل خطر (Risk Factors) برای بیماری‌ها"),
                   tags$li("ارزیابی دقت تست‌های تشخیصی (حساسیت و ویژگی)"),
                   tags$li("تعیین پروتکل‌های درمانی بر اساس شواهد"),
                   tags$li("پایش کیفیت خدمات سلامت"),
                   tags$li("تحلیل هزینه-اثربخشی مداخلات درمانی")
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱.۲.۲"), "کاربردهای تحقیقاتی"
                 ),
                 tags$ul(
                   tags$li("طراحی مطالعات پژوهشی"),
                   tags$li("تعیین حجم نمونه مناسب"),
                   tags$li("تحلیل داده‌های تحقیقاتی"),
                   tags$li("ارزیابی نتایج مطالعات"),
                   tags$li("تهیه گزارش‌های علمی")
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱.۳"), "اهمیت آمار در تصمیم‌گیری پزشکی"
                 ),
                 p("آمار به پزشکان و پژوهشگران کمک می‌کند تا:"),
                 tags$ul(
                   tags$li("تصمیمات مبتنی بر شواهد بگیرند"),
                   tags$li("خطاهای تشخیصی را کاهش دهند"),
                   tags$li("منابع را بهینه تخصیص دهند"),
                   tags$li("کیفیت خدمات را بهبود بخشند")
                 )
             )
           ),
           
           "variables" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۲: انواع متغیرها و نمونه‌گیری"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۲.۱"), "مفاهیم پایه: جامعه و نمونه"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۲.۱.۱"), "جامعه (Population)"
                 ),
                 div(class = "highlight-box",
                     p("جامعه به کل مجموعه افراد، اشیاء یا رویدادهایی گفته می‌شود که می‌خواهیم درباره آنها مطالعه کنیم."),
                     tags$ul(
                       tags$li(tags$b("جامعه هدف (Target Population):"), "جامعه‌ای که می‌خواهیم نتایج مطالعه را به آن تعمیم دهیم"),
                       tags$li(tags$b("جامعه در دسترس (Accessible Population):"), "بخشی از جامعه که واقعاً می‌توانیم به آن دسترسی داشته باشیم"),
                       tags$li(tags$b("مثال‌های پزشکی:"),
                               tags$ul(
                                 tags$li("تمام بیماران دیابتی در ایران"),
                                 tags$li("همه زنان باردار در یک استان"),
                                 tags$li("تمام پرونده‌های پزشکی یک بیمارستان در ۵ سال گذشته")
                               )
                       )
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۲.۱.۲"), "نمونه (Sample)"
                 ),
                 div(class = "success-box",
                     p("نمونه زیرمجموعه‌ای از جامعه است که برای مطالعه انتخاب می‌شود."),
                     tags$ul(
                       tags$li(tags$b("نمونه نماینده (Representative Sample):"), "نمونه‌ای که ویژگی‌های جامعه را به خوبی منعکس کند"),
                       tags$li(tags$b("حجم نمونه (Sample Size):"), "تعداد اعضای نمونه"),
                       tags$li(tags$b("مثال‌های پزشکی:"),
                               tags$ul(
                                 tags$li("۲۰۰ بیمار دیابتی از ۵ بیمارستان مختلف"),
                                 tags$li("۵۰ زن باردار از مراکز بهداشتی یک شهر"),
                                 tags$li("۱۰۰ پرونده پزشکی به صورت تصادفی انتخاب شده")
                               )
                       )
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۲.۱.۳"), "سرشماری در مقابل نمونه‌گیری"
                 ),
                 div(class = "warning-box",
                     fluidRow(
                       column(6,
                              h5("🎯 سرشماری (Census)"),
                              tags$ul(
                                tags$li("مطالعه تمام اعضای جامعه"),
                                tags$li(tags$b("مزایا:"),
                                        tags$ul(
                                          tags$li("دقت بسیار بالا"),
                                          tags$li("بدون خطای نمونه‌گیری")
                                        )),
                                tags$li(tags$b("معایب:"),
                                        tags$ul(
                                          tags$li("هزینه و زمان زیاد"),
                                          tags$li("امکان‌پذیر نبودن برای جامعه‌های بسیار بزرگ"),
                                          tags$li("مثال: سرشماری ملی هر ۱۰ سال یکبار")
                                        ))
                              )
                       ),
                       column(6,
                              h5("📊 نمونه‌گیری (Sampling)"),
                              tags$ul(
                                tags$li("مطالعه بخشی از جامعه"),
                                tags$li(tags$b("مزایا:"),
                                        tags$ul(
                                          tags$li("صرفه‌جویی در هزینه و زمان"),
                                          tags$li("امکان مطالعه جامعه‌های بزرگ"),
                                          tags$li("دقت کافی با روش‌های صحیح")
                                        )),
                                tags$li(tags$b("معایب:"),
                                        tags$ul(
                                          tags$li("خطای نمونه‌گیری"),
                                          tags$li("نیاز به روش‌های دقیق برای نمایا بودن")
                                        ))
                              )
                       )
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۲.۲"), "انواع متغیرها"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۲.۲.۱"), "دسته‌بندی متغیرها"
                 ),
                 tableOutput("variables_table"),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۲.۲.۲"), "متغیرهای کیفی"
                 ),
                 fluidRow(
                   column(6,
                          h4("متغیرهای کیفی (Qualitative)"),
                          tags$ul(
                            tags$li(tags$b("اسمی (Nominal):"), "بدون ترتیب طبیعی - مثال: گروه خونی، جنسیت"),
                            tags$li(tags$b("ترتیبی (Ordinal):"), "با ترتیب طبیعی - مثال: درجه سرطان، سطح درد")
                          )
                   ),
                   column(6,
                          h4("متغیرهای کمی"),
                          tags$ul(
                            tags$li(tags$b("فاصله‌ای (Interval):"), "مقادیر عددی با فاصله‌های معنی‌دار - مثال: دمای سانتیگراد"),
                            tags$li(tags$b("نسبی (Ratio):"), "مقادیر عددی با صفر مطلق - مثال: قد، وزن، فشار خون")
                          )
                   )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۲.۲.۳"), "مثال‌های پزشکی"
                 ),
                 fluidRow(
                   column(3,
                          h5("متغیرهای کیفی اسمی"),
                          tags$ul(
                            tags$li("گروه خونی"),
                            tags$li("جنسیت"),
                            tags$li("نوع بیماری")
                          )
                   ),
                   column(3,
                          h5("متغیرهای کیفی ترتیبی"),
                          tags$ul(
                            tags$li("درجه سرطان"),
                            tags$li("سطح درد"),
                            tags$li("رضایت بیمار")
                          )
                   ),
                   column(3,
                          h5("متغیرهای کمی فاصله‌ای"),
                          tags$ul(
                            tags$li("نمره آپگار"),
                            tags$li("امتیاز کیفیت زندگی"),
                            tags$li("دمای بدن")
                          )
                   ),
                   column(3,
                          h5("متغیرهای کمی نسبی"),
                          tags$ul(
                            tags$li("سن بیمار"),
                            tags$li("فشار خون"),
                            tags$li("سطح قند خون")
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۲.۳"), "روش‌های نمونه‌گیری"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۲.۳.۱"), "نمونه‌گیری احتمالی"
                 ),
                 div(class = "highlight-box",
                     h4("نمونه‌گیری احتمالی (Probability Sampling)"),
                     tags$ul(
                       tags$li(tags$b("تصادفی ساده (Simple Random):"), "هر عضو جامعه شانس برابر برای انتخاب دارد"),
                       tags$li(tags$b("طبقه‌ای (Stratified):"), "جامعه به طبقات تقسیم و از هر طبقه نمونه گرفته می‌شود"),
                       tags$li(tags$b("خوشه‌ای (Cluster):"), "خوشه‌هایی انتخاب و تمام اعضای خوشه بررسی می‌شوند"),
                       tags$li(tags$b("سیستماتیک (Systematic):"), "انتخاب نمونه‌ها با فاصله معین از لیست")
                     ),
                     p(tags$b("✅ مزیت:"), "امکان تعمیم نتایج به جامعه اصلی")
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۲.۳.۲"), "نمونه‌گیری غیراحتمالی"
                 ),
                 div(class = "warning-box",
                     h4("نمونه‌گیری غیراحتمالی (Non-Probability Sampling)"),
                     tags$ul(
                       tags$li(tags$b("در دسترس (Convenience):"), "نمونه‌های در دسترس انتخاب می‌شوند"),
                       tags$li(tags$b("هدفمند (Purposive):"), "نمونه‌ها با ویژگی‌های خاص انتخاب می‌شوند"),
                       tags$li(tags$b("گلوله برفی (Snowball):"), "از طریق معرفی نمونه‌های موجود")
                     ),
                     p(tags$b("⚠️ هشدار:"), "نمونه‌گیری غیراحتمالی ممکن است باعث سوگیری (Bias) شود"),
                     p(tags$b("🎯 کاربرد:"), "مطالعات کیفی، پژوهش‌های مقدماتی")
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۲.۴"), "مثال کاربردی در پزشکی"
                 ),
                 div(class = "info-box",
                     h4("مطالعه شیوع فشار خون در تهران"),
                     tags$ul(
                       tags$li(tags$b("جامعه هدف:"), "تمام ساکنین بالای ۱۸ سال تهران"),
                       tags$li(tags$b("جامعه در دسترس:"), "مراجعه‌کنندگان به مراکز بهداشتی درمانی تهران"),
                       tags$li(tags$b("روش نمونه‌گیری:"), "نمونه‌گیری خوشه‌ای تصادفی"),
                       tags$li(tags$b("حجم نمونه:"), "۲۰۰۰ نفر"),
                       tags$li(tags$b("متغیرها:"),
                               tags$ul(
                                 tags$li("فشار خون (کمی نسبی)"),
                                 tags$li("سن (کمی نسبی)"),
                                 tags$li("جنسیت (کیفی اسمی)"),
                                 tags$li("سابقه خانوادگی (کیفی اسمی)")
                               )
                       )
                     )
                 )
             )
           ),
           
           "descriptive" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۳: آمار توصیفی"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۳.۱"), "مقدمه آمار توصیفی"
                 ),
                 div(class = "highlight-box",
                     h4("تعریف آمار توصیفی"),
                     p("آمار توصیفی به مجموعه روش‌هایی گفته می‌شود که برای خلاصه‌سازی، سازماندهی و نمایش داده‌ها به کار می‌روند. هدف اصلی آمار توصیفی، توصیف ویژگی‌های اصلی داده‌ها به صورت روشن و معنادار است."),
                     tags$ul(
                       tags$li(tags$b("خلاصه‌سازی داده‌ها:"), "استفاده از شاخص‌های مرکزی و پراکندگی"),
                       tags$li(tags$b("نمایش داده‌ها:"), "استفاده از نمودارها و جداول"),
                       tags$li(tags$b("سازماندهی داده‌ها:"), "دسته‌بندی و مرتب‌سازی داده‌ها")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۳.۲"), "شناسایی مقادیر پرت"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۲.۱"), "مقادیر پرت چیست؟"
                 ),
                 div(class = "warning-box",
                     p("مقادیر پرت به داده‌هایی گفته می‌شود که به طور قابل توجهی با سایر داده‌ها تفاوت دارند. این مقادیر می‌توانند:"),
                     tags$ul(
                       tags$li("ناشی از خطای اندازه‌گیری باشند"),
                       tags$li("نشان‌دهنده یک پدیده نادر اما واقعی باشند"),
                       tags$li("بر نتایج تحلیل آماری تأثیر بگذارند")
                     ),
                     
                     h5("🎯 مثال‌های پزشکی از مقادیر پرت:"),
                     tags$ul(
                       tags$li(tags$b("فشار خون:"), "مقدار ۲۲۰/۱۳۰ mmHg در یک فرد جوان سالم"),
                       tags$li(tags$b("دمای بدن:"), "مقدار ۴۲ درجه سانتیگراد"),
                       tags$li(tags$b("سطح قند خون:"), "مقدار ۵۰۰ mg/dL در فرد ناشتا"),
                       tags$li(tags$b("وزن نوزاد:"), "۶ کیلوگرم در بدو تولد"),
                       tags$li(tags$b("ضربان قلب:"), "۲۰ ضربه در دقیقه در فرد بیدار")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۲.۲"), "روش‌های شناسایی مقادیر پرت"
                 ),
                 fluidRow(
                   column(6,
                          h5("روش‌های آماری:"),
                          tags$ul(
                            tags$li(tags$b("قاعده IQR:"), "مقادیر خارج از Q1 - 1.5×IQR و Q3 + 1.5×IQR"),
                            tags$li(tags$b("مقادیر Z-Score:"), "مقادیر با |Z| > 3"),
                            tags$li(tags$b("مقادیر 3 انحراف معیار:"), "خارج از μ ± 3σ")
                          )
                   ),
                   column(6,
                          h5("روش‌های گرافیکی:"),
                          tags$ul(
                            tags$li("نمودار جعبه‌ای (Boxplot)"),
                            tags$li("نمودار پراکندگی (Scatter Plot)"),
                            tags$li("هیستوگرام")
                          )
                   )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۲.۳"), "مثال کاربردی پزشکی"
                 ),
                 div(class = "success-box",
                     h5("کنترل کیفیت تست قند خون با قاعده 6 سیگما"),
                     tags$ul(
                       tags$li("میانگین هدف: 100 mg/dL"),
                       tags$li("انحراف معیار قابل قبول: 5 mg/dL"),
                       tags$li("محدوده قابل قبول (2σ): [90, 110] mg/dL"),
                       tags$li("محدوده هشدار (3σ): [85, 115] mg/dL"),
                       tags$li("محدوده اقدام (6σ): [70, 130] mg/dL")
                     ),
                     
                     h5("داده‌های روزانه:"),
                     tableOutput("sigma_lab_data"),
                     
                     h5("تفسیر:"),
                     tags$ul(
                       tags$li("✅ مقادیر 95, 102, 98: در محدوده قابل قبول"),
                       tags$li("⚠️ مقدار 116: در محدوده هشدار (بازبینی نیاز است)"),
                       tags$li("❌ مقدار 135: در محدوده اقدام (کالیبراسیون دستگاه نیاز است)")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۳.۳"), "شاخص‌های مرکزی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۳.۱"), "میانگین (Mean)"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              p("مجموع مقادیر تقسیم بر تعداد آنها"),
                              p(tags$b("فرمول:"), "x̄ = Σxᵢ / n"),
                              tags$ul(
                                tags$li(tags$b("ویژگی:"), "حساس به مقادیر پرت"),
                                tags$li(tags$b("مزایا:"), "استفاده از تمام داده‌ها"),
                                tags$li(tags$b("معایب"),"تحت تاثیر مقادیر پرت"),
                                tags$li(tags$b("مثال پزشکی:"), "میانگین فشار خون 120 بیمار")
                              )
                          )
                   ),
                   column(6,
                          plotOutput("mean_plot", height = "300px")
                   )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۳.۲"), "میانه (Median)"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              p("مقدار وسطی وقتی داده‌ها به ترتیب مرتب شده باشند"),
                              tags$ul(
                                tags$li(tags$b("ویژگی:"), "مقاوم به مقادیر پرت"),
                                tags$li(tags$b("مزایا:"), "مناسب برای داده‌های skewed"),
                                tags$li(tags$b("معایب:"), "عدم استفاده از تمام اطلاعات"),
                                tags$li(tags$b("مثال پزشکی:"), "میانه درآمد بیماران یک کلینیک")
                              )
                          )
                   ),
                   column(6,
                          plotOutput("median_plot", height = "300px")
                   )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۳.۳"), "نما (Mode)"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              p("پرتکرارترین مقدار در مجموعه داده"),
                              tags$ul(
                                tags$li(tags$b("ویژگی:"), "برای داده‌های کیفی و کمی کاربرد دارد"),
                                tags$li(tags$b("مزایا:"), "قابل استفاده برای داده‌های اسمی"),
                                tags$li(tags$b("معایب:"), "ممکن است چندین نما وجود داشته باشد"),
                                tags$li(tags$b("مثال پزشکی:"), "شایع‌ترین گروه خونی در یک جامعه")
                              )
                          )
                   ),
                   column(6,
                          plotOutput("mode_plot", height = "300px")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۳.۴"), "شاخص‌های پراکندگی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۴.۱"), "دامنه (Range)"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "warning-box",
                              p("تفاوت بین بزرگترین و کوچکترین مقدار"),
                              p(tags$b("فرمول:"), "R = Max - Min"),
                              tags$ul(
                                tags$li(tags$b("مزایا:"), "محاسبه ساده"),
                                tags$li(tags$b("معایب:"), "حساس به مقادیر پرت"),
                                tags$li(tags$b("کاربرد:"), "بررسی اولیه پراکندگی")
                              )
                          )
                   ),
                   column(6,
                          plotOutput("range_plot", height = "300px")
                   )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۴.۲"), "واریانس (Variance)"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "warning-box",
                              p("میانگین مربعات انحراف از میانگین"),
                              p(tags$b("فرمول:"), "s² = Σ(xᵢ - x̄)² / (n-1)"),
                              tags$ul(
                                tags$li(tags$b("ویژگی:"), "در واحد مربع متغیر بیان می‌شود"),
                                tags$li(tags$b("مزایا:"), "استفاده از تمام داده‌ها"),
                                tags$li(tags$b("معایب:"), "واحد سنجش نامأنوس")
                              )
                          )
                   ),
                   column(6,
                          plotOutput("variance_plot", height = "300px")
                   )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۴.۳"), "انحراف معیار (Standard Deviation)"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "warning-box",
                              p("جذر واریانس - نشان‌دهنده پراکندگی حول میانگین"),
                              p(tags$b("فرمول:"), "s = √s²"),
                              tags$ul(
                                tags$li(tags$b("مزایا:"), "واحد سنجش معنی‌دار"),
                                tags$li(tags$b("کاربرد:"), "مقایسه پراکندگی گروه‌های مختلف"),
                                tags$li(tags$b("مثال:"), "انحراف معیار فشار خون = 15 mmHg")
                              )
                          )
                   ),
                   column(6,
                          plotOutput("sd_plot", height = "300px")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۳.۵"), "چارک‌ها و مقادیر موقعیتی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۵.۱"), "چارک‌ها و دامنه میان چارکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              h5("چارک‌ها (Quartiles)"),
                              tags$ul(
                                tags$li(tags$b("چارک اول (Q1):"), "25% داده‌ها زیر این مقدار"),
                                tags$li(tags$b("چارک دوم (Q2):"), "میانه - 50% داده‌ها"),
                                tags$li(tags$b("چارک سوم (Q3):"), "75% داده‌ها زیر این مقدار")
                              ),
                              h5("دامنه میان چارکی (IQR)"),
                              p(tags$b("فرمول:"), "IQR = Q3 - Q1"),
                              p(tags$b("کاربرد:"), "شناسایی مقادیر پرت و توصیف پراکندگی")
                          )
                   ),
                   column(6,
                          plotOutput("quartile_plot", height = "300px")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۳.۶"), "جداول و نمودارهای توصیفی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۶.۱"), "جداول فراوانی"
                 ),
                 
                 div(class = "subsubsection-title",
                     span(class = "subsection-number", "۳.۶.۱.۱"), "جدول فراوانی برای داده‌های کیفی اسمی"
                 ),
                 div(class = "highlight-box",
                     p("برای متغیرهای کیفی، جدول فراوانی شامل موارد زیر است:"),
                     tags$ul(
                       tags$li(tags$b("فراوانی مطلق (Absolute Frequency):"), "تعداد مشاهده هر دسته"),
                       tags$li(tags$b("فراوانی نسبی (Relative Frequency):"), "نسبت هر دسته به کل داده‌ها"),
                       tags$li(tags$b("فراوانی درصدی (Percentage Frequency):"), "فراوانی نسبی ضرب در ۱۰۰")
                     ),
                     tableOutput("qualitative_freq_table"),
                     p(tags$b("مثال پزشکی:"), "توزیع گروه‌های خونی در یک نمونه ۱۰۰ نفره")
                 ),
                 
                 div(class = "subsubsection-title",
                     span(class = "subsection-number", "۳.۶.۱.۲"), "جدول فراوانی برای داده‌های کیفی ترتیبی"
                 ),
                 div(class = "info-box",
                     h4("جدول فراوانی برای درجه سرطان"),
                     p("برای متغیرهای کیفی ترتیبی، فراوانی تجمعی معنی‌دار است:"),
                     tags$ul(
                       tags$li(tags$b("ترتیبی (Ordinal):"), "با ترتیب طبیعی - مثال: درجه سرطان، سطح درد"),
                       tags$li("فراوانی تجمعی نشان می‌دهد چند بیمار در آن درجه یا درجات پایین‌تر هستند")
                     ),
                     tableOutput("ordinal_freq_table"),
                     p(tags$b("مثال پزشکی:"), "توزیع درجات سرطان در یک نمونه ۹۰ نفره")
                 ),
                 
                 div(class = "subsubsection-title",
                     span(class = "subsection-number", "۳.۶.۱.۳"), "جدول فراوانی برای داده‌های کمی"
                 ),
                 div(class = "success-box",
                     p("برای متغیرهای کمی، ابتدا داده‌ها را به بازه‌هایی تقسیم می‌کنیم:"),
                     tags$ul(
                       tags$li("تعیین تعداد بازه‌ها (معمولاً ۵-۱۵ بازه)"),
                       tags$li("محاسبه عرض بازه‌ها"),
                       tags$li("شمارش فراوانی هر بازه"),
                       tags$li("محاسبه نقطه میانی هر بازه")
                     ),
                     tableOutput("quantitative_freq_table"),
                     p(tags$b("مثال پزشکی:"), "توزیع سنی بیماران یک بیمارستان")
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۶.۲"), "نمودارهای توصیفی"
                 ),
                 fluidRow(
                   column(6,
                          h4("نمودارهای برای داده‌های کیفی"),
                          div(class = "info-box",
                              h5("نمودار میله‌ای (Bar Chart)"),
                              p("برای مقایسه فراوانی دسته‌های مختلف"),
                              p(tags$b("کاربرد:"), "مقایسه شیوع بیماری‌ها، گروه‌های خونی"),
                              plotOutput("bar_chart_demo", height = "200px")
                          ),
                          div(class = "info-box",
                              h5("نمودار دایره‌ای (Pie Chart)"),
                              p("برای نمایش سهم هر دسته از کل"),
                              p(tags$b("کاربرد:"), "نمایش ترکیب جمعیتی، توزیع عوامل خطر"),
                              plotOutput("pie_chart_demo", height = "200px")
                          )
                   ),
                   column(6,
                          h4("نمودارهای برای داده‌های کمی"),
                          div(class = "info-box",
                              h5("هیستوگرام (Histogram)"),
                              p("نمایش توزیع فراوانی داده‌های کمی"),
                              p(tags$b("کاربرد:"), "توزیع فشار خون، سن، وزن"),
                              plotOutput("histogram_demo", height = "200px")
                          ),
                          div(class = "info-box",
                              h5("نمودار جعبه‌ای (Boxplot)"),
                              p("نمایش چارک‌ها، میانه و مقادیر پرت"),
                              p(tags$b("کاربرد:"), "مقایسه گروه‌ها، شناسایی مقادیر پرت"),
                              plotOutput("boxplot_demo", height = "200px")
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۳.۷"), "مثال کاربردی: داده‌های فشار خون"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "warning-box",
                              h5("داده‌های خام فشار خون (نمونه‌ای)"),
                              tableOutput("bp_raw_data_table"),
                              p("با استفاده از این داده‌ها، جداول و نمودارهای زیر ساخته می‌شوند:")
                          )
                   ),
                   column(6,
                          div(class = "success-box",
                              h5("جدول فراوانی فشار خون"),
                              tableOutput("bp_freq_table"),
                              p("بازه‌ها: 10 mmHg intervals")
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۳.۸"), "خلاصه و نکات مهم"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۸.۱"), "خلاصه شاخص‌های آمار توصیفی"
                 ),
                 tableOutput("descriptive_summary_table"),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۸.۲"), "نکات مهم و هشدارها"
                 ),
                 div(class = "warning-box",
                     h4("اشتباهات رایج در آمار توصیفی"),
                     tags$ul(
                       tags$li("استفاده از میانگین برای داده‌های skewed"),
                       tags$li("تفسیر نادرست انحراف معیار"),
                       tags$li("بی‌توجهی به مقادیر پرت"),
                       tags$li("انتخاب نادرست نمودار برای نوع داده"),
                       tags$li("عدم گزارش شاخص‌های پراکندگی همراه با شاخص‌های مرکزی")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۳.۸.۳"), "راهنمای انتخاب شاخص‌های مناسب"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              h5("برای داده‌های نرمال"),
                              tags$ul(
                                tags$li("میانگین ± انحراف معیار"),
                                tags$li("محدوده (اختیاری)"),
                                tags$li("هیستوگرام + منحنی نرمال")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("برای داده‌های غیرنرمال"),
                              tags$ul(
                                tags$li("میانه (دامنه میان چارکی)"),
                                tags$li("مقادیر مینیمم و ماکسیمم"),
                                tags$li("نمودار جعبه‌ای")
                              )
                          )
                   )
                 )
             )
           ),
           
           "distributions" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۴: توزیع‌های آماری"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۱"), "مقدمه توزیع‌های آماری"
                 ),
                 div(class = "highlight-box",
                     h4("تعریف توزیع آماری"),
                     p("توزیع آماری توصیف می‌کند که چگونه مقادیر یک متغیر در جامعه توزیع شده‌اند. به عبارت دیگر، توزیع آماری نشان می‌دهد که چه مقادیری برای یک متغیر ممکن است رخ دهند و هر کدام با چه احتمالی ظاهر می‌شوند."),
                     tags$ul(
                       tags$li(tags$b("متغیر تصادفی (Random Variable):"), "متغیری که مقادیر آن به صورت تصادفی تعیین می‌شود"),
                       tags$li(tags$b("توزیع احتمال (Probability Distribution):"), "تابعی که احتمال رخ دادن هر مقدار را مشخص می‌کند"),
                       tags$li(tags$b("پارامترهای توزیع (Distribution Parameters):"), "مقادیری که شکل و ویژگی‌های توزیع را تعیین می‌کنند")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۲"), "اهمیت توزیع‌های آماری"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h5("در آمار توصیفی"),
                              tags$ul(
                                tags$li("خلاصه‌سازی داده‌ها"),
                                tags$li("تشخیص الگوها"),
                                tags$li("شناسایی مقادیر پرت"),
                                tags$li("درک رفتار متغیرها")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("در آمار استنباطی"),
                              tags$ul(
                                tags$li("انجام آزمون‌های فرضیه"),
                                tags$li("محاسبه فاصله اطمینان"),
                                tags$li("پیش‌بینی و مدل‌سازی"),
                                tags$li("تعمیم نتایج نمونه به جامعه")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۳"), "توزیع نرمال"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۴.۳.۱"), "ویژگی‌های توزیع نرمال"
                 ),
                 div(class = "highlight-box",
                     tags$ul(
                       tags$li("شکل زنگوله‌ای و متقارن (Bell-shaped and Symmetric)"),
                       tags$li("میانگین = میانه = نما (Mean = Median = Mode)"),
                       tags$li("توسط دو پارامتر میانگین (μ) و انحراف معیار (σ) تعریف می‌شود"),
                       tags$li("قانون 68-95-99.7 (Empirical Rule):"),
                       tags$ul(
                         tags$li("68% داده‌ها در μ ± σ"),
                         tags$li("95% داده‌ها در μ ± 2σ"),
                         tags$li("99.7% داده‌ها در μ ± 3σ")
                       )
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۴.۳.۲"), "کاربردهای پزشکی توزیع نرمال"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li("قد، وزن"),
                       tags$li("فشار خون"),
                       tags$li("سطح قند خون ناشتا"),
                       tags$li("میزان کلسترول"),
                       tags$li("دمای بدن در افراد سالم"),
                       tags$li("ضربان قلب در حالت استراحت")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۴"), "توزیع دوجمله‌ای"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۴.۴.۱"), "ویژگی‌های توزیع دوجمله‌ای"
                 ),
                 div(class = "info-box",
                     tags$ul(
                       tags$li("تعداد موفقیت‌ها در n آزمایش"),
                       tags$li("پارامترها: n (تعداد آزمایش) و p (احتمال موفقیت)"),
                       tags$li("میانگین: np"),
                       tags$li("واریانس: np(1-p)"),
                       tags$li("مثال‌های پزشکی:"),
                       tags$ul(
                         tags$li("تعداد بهبودی پس از درمان"),
                         tags$li("تعداد پاسخ مثبت به دارو"),
                         tags$li("تعداد تشخیص‌های صحیح")
                       )
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۵"), "توزیع پواسون"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۴.۵.۱"), "ویژگی‌های توزیع پواسون"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li("برای رویدادهای نادر در واحد زمان یا مکان"),
                       tags$li("میانگین = واریانس (λ)"),
                       tags$li("رویدادها مستقل از هم رخ می‌دهند"),
                       tags$li("مثال‌های پزشکی:"),
                       tags$ul(
                         tags$li("تعداد بیماران اورژانس در ساعت"),
                         tags$li("تعداد خطاهای پزشکی در ماه"),
                         tags$li("تعداد تولدهای دوقلو در یک بیمارستان"),
                         tags$li("تعداد موارد یک بیماری نادر در سال"),
                         tags$li("تعداد عفونت‌های بیمارستانی در هفته")
                       )
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۶"), "شبیه‌ساز توزیع نرمال"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h5("تنظیمات توزیع نرمال"),
                              sliderInput("norm_mean", "میانگین (μ):", 
                                          min = 0, max = 200, value = 100, step = 1),
                              sliderInput("norm_sd", "انحراف معیار (σ):", 
                                          min = 1, max = 50, value = 15, step = 1),
                              numericInput("norm_sample_size", "تعداد نمونه:", 
                                           value = 1000, min = 100, max = 10000),
                              actionButton("plot_normal", "نمایش توزیع")
                          )
                   ),
                   column(8,
                          plotOutput("normal_plot", height = "400px"),
                          verbatimTextOutput("normal_info")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۷"), "شبیه‌ساز توزیع دوجمله‌ای"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h5("تنظیمات توزیع دوجمله‌ای"),
                              sliderInput("binom_n", "تعداد آزمایش (n):", 
                                          min = 1, max = 100, value = 20),
                              sliderInput("binom_p", "احتمال موفقیت (p):", 
                                          min = 0, max = 1, value = 0.5, step = 0.05),
                              numericInput("binom_sample_size", "تعداد نمونه:", 
                                           value = 1000, min = 100, max = 10000),
                              actionButton("plot_binomial", "نمایش توزیع")
                          )
                   ),
                   column(8,
                          plotOutput("binomial_plot"),
                          verbatimTextOutput("binomial_info")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۸"), "شبیه‌ساز توزیع پواسون"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h5("تنظیمات توزیع پواسون"),
                              sliderInput("pois_lambda", "میانگین (λ):", 
                                          min = 0.1, max = 20, value = 5, step = 0.5),
                              numericInput("pois_sample_size", "تعداد نمونه:", 
                                           value = 1000, min = 100, max = 10000),
                              actionButton("plot_poisson", "نمایش توزیع")
                          )
                   ),
                   column(8,
                          plotOutput("poisson_plot"),
                          verbatimTextOutput("poisson_info")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۹"), "کاربردهای بالینی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۴.۹.۱"), "تشخیص نرمال بودن داده‌ها"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              tags$ul(
                                tags$li("آزمون شاپیرو-ویلک (Shapiro-Wilk test)"),
                                tags$li("نمودار Q-Q (Quantile-Quantile plot)"),
                                tags$li("بررسی چولگی و کشیدگی (Skewness & Kurtosis)"),
                                tags$li("مثال: بررسی نرمال بودن سطح قند خون بیماران")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("انتخاب آزمون آماری مناسب"),
                              tags$ul(
                                tags$li("داده‌های نرمال → آزمون‌های پارامتری"),
                                tags$li("داده‌های غیرنرمال → آزمون‌های ناپارامتری"),
                                tags$li("داده‌های دوحالتی → آزمون‌های مبتنی بر توزیع دوجمله‌ای"),
                                tags$li("داده‌های شمارشی → توزیع پواسون")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۱۰"), "مثال‌های کاربردی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۴.۱۰.۱"), "مثال ۱: مطالعه سطح کلسترول"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("متغیر:"), "سطح کلسترول LDL (کمی)"),
                       tags$li(tags$b("توزیع:"), "نرمال (با میانگین 130 و انحراف معیار 25)"),
                       tags$li(tags$b("سوال:"), "چند درصد بیماران کلسترول بالای 160 دارند؟"),
                       tags$li(tags$b("محاسبه:"), 
                               "Z = (160-130)/25 = 1.2 → احتمال = 11.5%"),
                       tags$li(tags$b("نتیجه:"), "حدود ۱۱.۵٪ بیماران کلسترول خطرناک دارند")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۴.۱۰.۲"), "مثال ۲: اثر بخشی داروی جدید"
                 ),
                 div(class = "info-box",
                     tags$ul(
                       tags$li(tags$b("متغیر:"), "تعداد بهبود یافته (دوحالتی)"),
                       tags$li(tags$b("توزیع:"), "دوجمله‌ای (n=100, p=0.7)"),
                       tags$li(tags$b("سوال:"), "احتمال بهبودی حداقل ۸۰ بیمار چقدر است؟"),
                       tags$li(tags$b("محاسبه:"), 
                               "P(X ≥ 80) = 1 - P(X ≤ 79) = 0.016"),
                       tags$li(tags$b("نتیجه:"), "احتمال بهبودی حداقل ۸۰ بیمار تنها ۱.۶٪ است")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۱۱"), "نکات مهم"
                 ),
                 div(class = "highlight-box",
                     h4("اشتباهات رایج"),
                     tags$ul(
                       tags$li("فرض نرمال بودن بدون بررسی"),
                       tags$li("استفاده از آزمون‌های پارامتری برای داده‌های غیرنرمال"),
                       tags$li("تفسیر نادرست پارامترهای توزیع"),
                       tags$li("بی‌توجهی به مقادیر پرت در تحلیل توزیع"),
                       tags$li("انتخاب نادرست توزیع برای نوع داده")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۴.۱۲"), "خلاصه توزیع‌های مهم"
                 ),
                 tableOutput("distributions_summary_table")
             )
           ),
           
           "tests_ci" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۵: آزمون فرض و فاصله اطمینان"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۱"), "مقدمه"
                 ),
                 div(class = "highlight-box",
                     h4("هدف آزمون فرض و فاصله اطمینان"),
                     p("این دو مفهوم پایه‌ای در آمار استنباطی هستند که به ما کمک می‌کنند از روی نمونه‌ها به نتیجه‌گیری درباره جامعه بپردازیم:"),
                     tags$ul(
                       tags$li(tags$b("آزمون فرض (Hypothesis Testing):"), "ارزیابی ادعاها درباره پارامترهای جامعه"),
                       tags$li(tags$b("فاصله اطمینان (Confidence Interval):"), "تخمین محدوده‌ای که پارامتر جامعه در آن قرار دارد")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۲"), "مفاهیم پایه در آزمون فرض"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۵.۲.۱"), "فرضیه‌ها"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li(tags$b("فرض صفر (H₀ - Null Hypothesis):"), 
                               "فرضیه‌ای که می‌خواهیم آن را آزمایش کنیم. معمولاً بیانگر 'عدم تفاوت' یا 'عدم اثر' است"),
                       tags$li(tags$b("فرض مقابل (H₁ - Alternative Hypothesis):"), 
                               "فرضیه‌ای که در صورت رد فرض صفر، آن را می‌پذیریم. بیانگر 'تفاوت' یا 'اثر' است")
                     ),
                     h5("مثال پزشکی:"),
                     p("H₀: داروی جدید اثری بر فشار خون ندارد"),
                     p("H₁: داروی جدید بر فشار خون اثر دارد")
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۳"), "انواع خطا در آزمون فرض"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۵.۳.۱"), "خطای نوع اول"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "warning-box",
                              h4("خطای نوع اول (Type I Error - α)"),
                              tags$ul(
                                tags$li("رد فرض صفر در حالی که درست است"),
                                tags$li("مشخص شده با α (آلفا)"),
                                tags$li(tags$b("سطح معنی‌داری (Significance Level):"), "حداکثر احتمال خطای نوع اول"),
                                tags$li("معمولاً 0.05 یا 0.01 در نظر گرفته می‌شود"),
                                tags$li(tags$b("مثال پزشکی:"), "تشخیص بیماری در فرد سالم")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("خطای نوع دوم (Type II Error - β)"),
                              tags$ul(
                                tags$li("پذیرش فرض صفر در حالی که نادرست است"),
                                tags$li("مشخص شده با β (بتا)"),
                                tags$li(tags$b("توان آزمون (Power):"), "1 - β (احتمال رد فرض صفر نادرست)"),
                                tags$li("معمولاً 0.2 یا 0.1 در نظر گرفته می‌شود"),
                                tags$li(tags$b("مثال پزشکی:"), "عدم تشخیص بیماری در فرد بیمار")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۴"), "خلاصه انواع خطا"
                 ),
                 tableOutput("error_types_table"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۵"), "مراحل انجام آزمون فرض"
                 ),
                 div(class = "highlight-box",
                     h4("پنج مرحله اصلی آزمون فرض"),
                     tags$ol(
                       tags$li(tags$b("تعیین فرضیه‌ها:"), "H₀ و H₁ را مشخص کنید"),
                       tags$li(tags$b("تعیین سطح معنی‌داری:"), "α را انتخاب کنید (معمولاً 0.05)"),
                       tags$li(tags$b("محاسبه آماره آزمون:"), "بر اساس داده‌های نمونه"),
                       tags$li(tags$b("تعیین ناحیه بحرانی:"), "مقادیری که منجر به رد H₀ می‌شوند"),
                       tags$li(tags$b("تصمیم‌گیری و نتیجه‌گیری:"), "رد یا عدم رد H₀ و تفسیر نتایج")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۶"), "مفهوم p-value"
                 ),
                 div(class = "success-box",
                     h4("p-value چیست؟"),
                     p("p-value احتمال مشاهده نتایج نمونه (یا نتایج افراطی‌تر) را در صورت صحیح بودن فرض صفر نشان می‌دهد."),
                     tags$ul(
                       tags$li(tags$b("p-value کوچک:"), "شواهد قوی علیه فرض صفر"),
                       tags$li(tags$b("p-value بزرگ:"), "شواهد ضعیف علیه فرض صفر"),
                       tags$li(tags$b("قاعده تصمیم:"), "اگر p-value < α باشد، H₀ را رد می‌کنیم")
                     ),
                     h5("تفسیر p-value:"),
                     tags$ul(
                       tags$li("p-value < 0.01: بسیار معنی‌دار"),
                       tags$li("0.01 ≤ p-value < 0.05: معنی‌دار"),
                       tags$li("0.05 ≤ p-value < 0.1: حاشیه‌ای"),
                       tags$li("p-value ≥ 0.1: غیر معنی‌دار")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۷"), "فاصله اطمینان"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۵.۷.۱"), "تعریف فاصله اطمینان"
                 ),
                 div(class = "info-box",
                     p("فاصله اطمینان محدوده‌ای از مقادیر است که با اطمینان مشخصی شامل پارامتر واقعی جامعه می‌شود."),
                     tags$ul(
                       tags$li(tags$b("سطح اطمینان (Confidence Level):"), "احتمال اینکه فاصله اطمینان پارامتر واقعی را شامل شود"),
                       tags$li(tags$b("حدود فاصله:"), "مقادیر حد پایین و حد بالای فاصله"),
                       tags$li(tags$b("عرض فاصله:"), "نشان‌دهنده دقت برآورد")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۵.۷.۲"), "رابطه آزمون فرض و فاصله اطمینان"
                 ),
                 div(class = "warning-box",
                     h4("ارتباط این دو مفهوم"),
                     p("آزمون فرض و فاصله اطمینان دو روی یک سکه هستند:"),
                     tags$ul(
                       tags$li("اگر فاصله اطمینان 95% شامل مقدار فرض صفر نباشد، آزمون فرض در سطح 0.05 معنی‌دار است"),
                       tags$li("اگر فاصله اطمینان شامل مقدار فرض صفر باشد، آزمون فرض معنی‌دار نیست"),
                       tags$li("فاصله اطمینان اطلاعات بیشتری ارائه می‌دهد: هم معنی‌داری و هم اندازه اثر")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۸"), "مثال‌های کاربردی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۵.۸.۱"), "مثال ۱: آزمون فرض"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "highlight-box",
                              tags$ul(
                                tags$li(tags$b("سوال:"), "آیا داروی جدید فشار خون را کاهش می‌دهد؟"),
                                tags$li(tags$b("H₀:"), "μ = 130 (میانگین فشار خون تغییر نمی‌کند)"),
                                tags$li(tags$b("H₁:"), "μ < 130 (میانگین فشار خون کاهش می‌یابد)"),
                                tags$li(tags$b("α:"), "0.05"),
                                tags$li(tags$b("نتایج:"), "p-value = 0.03"),
                                tags$li(tags$b("نتیجه:"), "رد H₀ - دارو اثر معنی‌دار دارد")
                              )
                          )
                   ),
                   column(6,
                          div(class = "success-box",
                              tags$ul(
                                tags$li(tags$b("سوال:"), "میانگین فشار خون جامعه چقدر است؟"),
                                tags$li(tags$b("نمونه:"), "میانگین = 125, انحراف معیار = 15, n = 100"),
                                tags$li(tags$b("فاصله اطمینان 95%:"), "122.1 تا 127.9"),
                                tags$li(tags$b("تفسیر:"), "با اطمینان 95%، میانگین فشار خون جامعه بین 122.1 و 127.9 است")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۹"), "شبیه‌ساز فاصله اطمینان"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h5("تنظیمات فاصله اطمینان"),
                              numericInput("ci_mean", "میانگین نمونه:", value = 100),
                              numericInput("ci_sd", "انحراف معیار نمونه:", value = 15),
                              numericInput("ci_n", "حجم نمونه:", value = 30),
                              selectInput("ci_level", "سطح اطمینان:",
                                          choices = c("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
                                          selected = "0.95"),
                              actionButton("calc_ci", "محاسبه فاصله اطمینان")
                          )
                   ),
                   column(8,
                          plotOutput("ci_plot"),
                          verbatimTextOutput("ci_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۱۰"), "شبیه‌ساز آزمون فرض"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h5("تنظیمات آزمون فرض"),
                              numericInput("ht_sample_mean", "میانگین نمونه:", value = 102),
                              numericInput("ht_pop_mean", "میانگین فرض صفر:", value = 100),
                              numericInput("ht_sd", "انحراف معیار:", value = 15),
                              numericInput("ht_n", "حجم نمونه:", value = 30),
                              selectInput("ht_alpha", "سطح معنی‌داری:",
                                          choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                                          selected = "0.05"),
                              actionButton("calc_ht", "انجام آزمون فرض")
                          )
                   ),
                   column(8,
                          plotOutput("ht_plot"),
                          verbatimTextOutput("ht_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۱۱"), "نکات مهم و هشدارها"
                 ),
                 div(class = "warning-box",
                     h4("اشتباهات رایج"),
                     tags$ul(
                       tags$li("تفسیر p-value به عنوان احتمال درست بودن فرض صفر"),
                       tags$li("معنی‌دار بودن آماری ≠ اهمیت بالینی"),
                       tags$li("بی‌توجهی به خطای نوع دوم در مطالعات با حجم نمونه کوچک"),
                       tags$li("استفاده نادرست از 'پذیرش فرض صفر' به جای 'عدم رد فرض صفر'"),
                       tags$li("تکیه صرف بر p-value بدون توجه به اندازه اثر")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۱۲"), "راهنمای عملی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              h5("در طراحی مطالعه"),
                              tags$ul(
                                tags$li("سطح معنی‌داری (α) را از قبل مشخص کنید"),
                                tags$li("توان آزمون (1-β) مناسب انتخاب کنید"),
                                tags$li("حجم نمونه کافی در نظر بگیرید"),
                                tags$li("فرضیه‌ها را به وضوح تعریف کنید")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("در گزارش نتایج"),
                              tags$ul(
                                tags$li("p-value دقیق گزارش دهید"),
                                tags$li("فاصله اطمینان ارائه دهید"),
                                tags$li("اندازه اثر را گزارش کنید"),
                                tags$li("نتایج را در context بالینی تفسیر کنید")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۵.۱۳"), "خلاصه مفاهیم کلیدی"
                 ),
                 tableOutput("key_concepts_table")
             )
           ),
           
           "statistical_tests" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۶: آزمون‌های آماری برای یک و دو گروه"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۱"), "مقدمه و طبقه‌بندی آزمون‌ها"
                 ),
                 div(class = "highlight-box",
                     h4("تفکیک آزمون‌های پارامتری و ناپارامتری"),
                     fluidRow(
                       column(6,
                              div(class = "success-box",
                                  h4("🎯 آزمون‌های پارامتری"),
                                  tags$ul(
                                    tags$li("برای داده‌های کمی با توزیع نرمال"),
                                    tags$li("قدرت آماری بالاتر"),
                                    tags$li("نیاز به بررسی پیش‌فرض‌ها"),
                                    tags$li("مثال: آزمون t، ANOVA")
                                  )
                              )
                       ),
                       column(6,
                              div(class = "warning-box",
                                  h4("🔄 آزمون‌های ناپارامتری"),
                                  tags$ul(
                                    tags$li("برای داده‌های کیفی یا کمی غیرنرمال"),
                                    tags$li("انعطاف‌پذیری بیشتر"),
                                    tags$li("نیاز به پیش‌فرض‌های کمتر"),
                                    tags$li("مثال: من-ویتنی، ویلکاکسون")
                                  )
                              )
                       )
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۲"), "پیش‌فرض‌های آزمون‌های پارامتری"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۲.۱"), "پیش‌فرض‌های اساسی"
                 ),
                 div(class = "warning-box",
                     h4("📋 چک‌لیست پیش‌فرض‌های پارامتری"),
                     tags$ul(
                       tags$li(tags$b("نرمال بودن داده‌ها (Normality):"),
                               tags$ul(
                                 tags$li("داده‌ها باید از توزیع نرمال پیروی کنند"),
                                 tags$li("روش بررسی: آزمون شاپیرو-ویلک، نمودار Q-Q"),
                                 tags$li("اقدام جایگزین: استفاده از آزمون ناپارامتری")
                               )),
                       tags$li(tags$b("همسانی واریانس‌ها (Homogeneity of Variance):"),
                               tags$ul(
                                 tags$li("واریانس گروه‌ها باید برابر باشد"),
                                 tags$li("روش بررسی: آزمون لوین، آزمون F"),
                                 tags$li("اقدام جایگزین: استفاده از نسخه تصحیح شده آزمون")
                               )),
                       tags$li(tags$b("استقلال مشاهدات (Independence):"),
                               tags$ul(
                                 tags$li("داده‌ها باید از هم مستقل باشند"),
                                 tags$li("روش بررسی: طراحی مطالعه مناسب"),
                                 tags$li("اقدام جایگزین: استفاده از مدل‌های وابسته")
                               )),
                       tags$li(tags$b("خطی بودن (Linearity):"),
                               tags$ul(
                                 tags$li("برای رگرسیون - رابطه بین متغیرها خطی باشد"),
                                 tags$li("روش بررسی: نمودار پراکندگی"),
                                 tags$li("اقدام جایگزین: تبدیل داده یا مدل غیرخطی")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۳"), "آزمون‌های برای یک گروه"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۳.۱"), "آزمون t تک نمونه‌ای (پارامتری)"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه میانگین یک گروه با مقدار ثابت"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: μ = μ₀ (میانگین برابر مقدار مورد انتظار است)"),
                                 tags$li("H₁: μ ≠ μ₀ (میانگین با مقدار مورد انتظار تفاوت دارد)")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمی و پیوسته باشند"),
                                 tags$li("داده‌ها از توزیع نرمال پیروی کنند"),
                                 tags$li("مشاهدات مستقل باشند")
                               )),
                       tags$li(tags$b("روش بررسی پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("نرمال بودن: آزمون شاپیرو-ویلک یا نمودار Q-Q"),
                                 tags$li("نمونه تصادفی و مستقل")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "مقایسه میانگین فشار خون بیماران با مقدار نرمال 120 mmHg")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۳.۲"), "آزمون علامت (ناپارامتری)"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه میانه یک گروه با مقدار ثابت"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: میانه = مقدار مورد انتظار"),
                                 tags$li("H₁: میانه ≠ مقدار مورد انتظار")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمی باشند"),
                                 tags$li("نیاز به توزیع نرمال ندارد"),
                                 tags$li("مشاهدات مستقل باشند"),
                                 tags$li("مقیاس حداقل ترتیبی")
                               )),
                       tags$li(tags$b("مزایا:"),
                               tags$ul(
                                 tags$li("مقاوم به مقادیر پرت"),
                                 tags$li("نیاز به پیش‌فرض‌های کم")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "مقایسه میانه سطح درد بیماران با مقدار آستانه")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۴"), "آزمون‌های برای دو گروه مستقل"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۴.۱"), "آزمون t مستقل (پارامتری)"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه میانگین دو گروه مستقل"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: μ₁ = μ₂ (میانگین دو گروه برابر است)"),
                                 tags$li("H₁: μ₁ ≠ μ₂ (میانگین دو گروه تفاوت دارد)")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمی و پیوسته باشند"),
                                 tags$li("داده‌ها در هر گروه نرمال باشند"),
                                 tags$li("واریانس دو گروه برابر باشد (همگنی واریانس)"),
                                 tags$li("مشاهدات مستقل باشند")
                               )),
                       tags$li(tags$b("روش بررسی پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("نرمال بودن: آزمون شاپیرو-ویلک برای هر گروه"),
                                 tags$li("همسانی واریانس: آزمون لوین"),
                                 tags$li("نمونه‌گیری مستقل")
                               )),
                       tags$li(tags$b("اقدام در صورت نقض پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("نقض نرمال بودن: استفاده از من-ویتنی"),
                                 tags$li("نقض همسانی واریانس: استفاده از آزمون t با واریانس نابرابر")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "مقایسه میانگین فشار خون بین زنان و مردان")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۴.۲"), "آزمون من-ویتنی (ناپارامتری)"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه توزیع دو گروه مستقل"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: توزیع دو گروه یکسان است"),
                                 tags$li("H₁: توزیع دو گروه تفاوت دارد")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمی یا ترتیبی باشند"),
                                 tags$li("نیاز به توزیع نرمال ندارد"),
                                 tags$li("مشاهدات مستقل باشند"),
                                 tags$li("مقیاس حداقل ترتیبی")
                               )),
                       tags$li(tags$b("مزایا:"),
                               tags$ul(
                                 tags$li("مقاوم به مقادیر پرت"),
                                 tags$li("قدرت خوب برای داده‌های غیرنرمال"),
                                 tags$li("نیاز به پیش‌فرض‌های کم")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "مقایسه سطح درد بین دو روش درمانی")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۵"), "آزمون‌های برای دو گروه وابسته"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۵.۱"), "آزمون t زوجی (پارامتری)"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه میانگین یک گروه در دو زمان مختلف"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: μ₁ = μ₂ (میانگین دو اندازه‌گیری برابر است)"),
                                 tags$li("H₁: μ₁ ≠ μ₂ (میانگین دو اندازه‌گیری تفاوت دارد)")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمی و پیوسته باشند"),
                                 tags$li("تفاضل جفت‌ها نرمال باشد"),
                                 tags$li("مشاهدات وابسته باشند (جفت‌شده)")
                               )),
                       tags$li(tags$b("روش بررسی پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("نرمال بودن تفاضل‌ها: آزمون شاپیرو-ویلک"),
                                 tags$li("جفت‌شدگی مناسب داده‌ها")
                               )),
                       tags$li(tags$b("اقدام در صورت نقض پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("نقض نرمال بودن تفاضل‌ها: استفاده از ویلکاکسون")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "مقایسه فشار خون بیماران قبل و بعد از درمان")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۵.۲"), "آزمون ویلکاکسون (ناپارامتری)"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه توزیع یک گروه در دو زمان مختلف"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: توزیع دو اندازه‌گیری یکسان است"),
                                 tags$li("H₁: توزیع دو اندازه‌گیری تفاوت دارد")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمی یا ترتیبی باشند"),
                                 tags$li("نیاز به توزیع نرمال ندارد"),
                                 tags$li("مشاهدات وابسته باشند"),
                                 tags$li("توزیع تفاضل‌ها متقارن باشد")
                               )),
                       tags$li(tags$b("مزایا:"),
                               tags$ul(
                                 tags$li("مقاوم به مقادیر پرت"),
                                 tags$li("مناسب برای داده‌های ترتیبی"),
                                 tags$li("نیاز به پیش‌فرض نرمال بودن ندارد")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "مقایسه سطح درد بیماران قبل و بعد از دارو")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۶"), "راهنمای جامع انتخاب آزمون"
                 ),
                 tableOutput("comprehensive_test_selection_guide"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۷"), "بررسی پیش‌فرض نرمال بودن"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۷.۱"), "روش‌های بررسی نرمال بودن"
                 ),
                 div(class = "info-box",
                     h4("📊 روش‌های تشخیص نرمال بودن داده‌ها"),
                     fluidRow(
                       column(6,
                              h5("روش‌های گرافیکی:"),
                              tags$ul(
                                tags$li(tags$b("نمودار Q-Q (Quantile-Quantile):"),
                                        tags$ul(
                                          tags$li("داده‌ها روی خط راست → نرمال"),
                                          tags$li("انحراف از خط → غیرنرمال")
                                        )),
                                tags$li(tags$b("هیستوگرام:"),
                                        tags$ul(
                                          tags$li("شکل زنگوله‌ای → نرمال"),
                                          tags$li("چولگی → غیرنرمال")
                                        )),
                                tags$li(tags$b("نمودار جعبه‌ای:"),
                                        tags$ul(
                                          tags$li("متقارن → نرمال"),
                                          tags$li("نامتقارن → غیرنرمال")
                                        ))
                              )
                       ),
                       column(6,
                              h5("روش‌های آماری:"),
                              tags$ul(
                                tags$li(tags$b("آزمون شاپیرو-ویلک:"),
                                        tags$ul(
                                          tags$li("مناسب برای نمونه‌های کوچک (n < 50)"),
                                          tags$li("H₀: داده‌ها نرمال هستند"),
                                          tags$li("p-value > 0.05 → نرمال")
                                        )),
                                tags$li(tags$b("آزمون کولموگروف-اسمیرنوف:"),
                                        tags$ul(
                                          tags$li("مناسب برای نمونه‌های بزرگ (n > 50)"),
                                          tags$li("H₀: داده‌ها از توزیع نرمال پیروی می‌کنند"),
                                          tags$li("p-value > 0.05 → نرمال")
                                        )),
                                tags$li(tags$b("آماره چولگی و کشیدگی:"),
                                        tags$ul(
                                          tags$li("چولگی ≈ 0 و کشیدگی ≈ 3 → نرمال"),
                                          tags$li("انحراف از این مقادیر → غیرنرمال")
                                        ))
                              )
                       )
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۸"), "شبیه‌ساز بررسی نرمال بودن"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h5("تنظیمات داده‌ها"),
                              sliderInput("norm_check_mean", "میانگین:", 
                                          min = 0, max = 200, value = 100),
                              sliderInput("norm_check_sd", "انحراف معیار:", 
                                          min = 1, max = 50, value = 15),
                              sliderInput("norm_check_n", "حجم نمونه:", 
                                          min = 10, max = 500, value = 100),
                              sliderInput("norm_check_skew", "درجه چولگی:", 
                                          min = -2, max = 2, value = 0, step = 0.1),
                              sliderInput("norm_check_kurtosis", "درجه کشیدگی:", 
                                          min = -1, max = 5, value = 0, step = 0.1),
                              actionButton("run_norm_check", "بررسی نرمال بودن")
                          )
                   ),
                   column(8,
                          plotOutput("norm_check_plot"),
                          verbatimTextOutput("norm_check_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۹"), "راهنمای تفسیر نتایج نرمالیتی"
                 ),
                 tableOutput("normality_decision_table"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۱۰"), "شبیه‌ساز آزمون t مستقل"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h5("تنظیمات گروه ۱"),
                              numericInput("group1_mean", "میانگین:", value = 100),
                              numericInput("group1_sd", "انحراف معیار:", value = 15),
                              numericInput("group1_n", "حجم نمونه:", value = 30),
                              
                              h5("تنظیمات گروه ۲"),
                              numericInput("group2_mean", "میانگین:", value = 110),
                              numericInput("group2_sd", "انحراف معیار:", value = 15),
                              numericInput("group2_n", "حجم نمونه:", value = 30),
                              
                              selectInput("test_alpha", "سطح معنی‌داری:",
                                          choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                                          selected = "0.05"),
                              actionButton("run_ttest", "انجام آزمون t")
                          )
                   ),
                   column(8,
                          plotOutput("ttest_plot"),
                          verbatimTextOutput("ttest_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۱۱"), "شبیه‌ساز آزمون من-ویتنی"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h5("داده‌های گروه‌ها"),
                              p("میانگین رتبه‌ها را وارد کنید:"),
                              numericInput("mw_group1_rank", "میانگین رتبه گروه ۱:", value = 25),
                              numericInput("mw_group2_rank", "میانگین رتبه گروه ۲:", value = 35),
                              numericInput("mw_n1", "حجم نمونه گروه ۱:", value = 30),
                              numericInput("mw_n2", "حجم نمونه گروه ۲:", value = 30),
                              actionButton("run_mwtest", "انجام آزمون من-ویتنی")
                          )
                   ),
                   column(8,
                          plotOutput("mwtest_plot"),
                          verbatimTextOutput("mwtest_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۱۲"), "مثال‌های کاربردی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۶.۱۲.۱"), "مثال ۱: انتخاب بین t-test و من-ویتنی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "highlight-box",
                              h5("📊 سناریو ۱: داده‌های نرمال"),
                              tags$ul(
                                tags$li(tags$b("داده‌ها:"), "سطح هموگلوبین بیماران (کمی)"),
                                tags$li(tags$b("بررسی نرمال بودن:"), "p-value شاپیرو = 0.12 → نرمال"),
                                tags$li(tags$b("همسانی واریانس:"), "p-value لوین = 0.08 → برقرار"),
                                tags$li(tags$b("آزمون انتخاب شده:"), "t-test مستقل"),
                                tags$li(tags$b("نتایج:"), "p-value = 0.03"),
                                tags$li(tags$b("نتیجه:"), "تفاوت معنی‌دار بین گروه‌ها")
                              )
                          )
                   ),
                   column(6,
                          div(class = "warning-box",
                              h5("🔄 سناریو ۲: داده‌های غیرنرمال"),
                              tags$ul(
                                tags$li(tags$b("داده‌ها:"), "سطح درد بیماران (ترتیبی)"),
                                tags$li(tags$b("بررسی نرمال بودن:"), "p-value شاپیرو = 0.008 → غیرنرمال"),
                                tags$li(tags$b("آزمون انتخاب شده:"), "من-ویتنی"),
                                tags$li(tags$b("نتایج:"), "p-value = 0.02"),
                                tags$li(tags$b("نتیجه:"), "تفاوت معنی‌دار در توزیع داده‌ها")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۱۳"), "اندازه اثر"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h5("برای آزمون t (پارامتری)"),
                              tags$ul(
                                tags$li(tags$b("d کوهن:"), "(میانگین۱ - میانگین۲) / انحراف معیار ترکیبی"),
                                tags$li(tags$b("تفسیر:"),
                                        tags$ul(
                                          tags$li("d = 0.2: اثر کوچک"),
                                          tags$li("d = 0.5: اثر متوسط"),
                                          tags$li("d = 0.8: اثر بزرگ")
                                        ))
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("برای آزمون‌های ناپارامتری"),
                              tags$ul(
                                tags$li(tags$b("r:"), "Z / √N"),
                                tags$li(tags$b("تفسیر:"),
                                        tags$ul(
                                          tags$li("r = 0.1: اثر کوچک"),
                                          tags$li("r = 0.3: اثر متوسط"),
                                          tags$li("r = 0.5: اثر بزرگ")
                                        ))
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۱۴"), "خلاصه آزمون‌ها"
                 ),
                 tableOutput("parametric_nonparametric_summary_table"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۱۵"), "نکات عملی و هشدارها"
                 ),
                 div(class = "warning-box",
                     h4("⚠️ هشدارهای مهم"),
                     tags$ul(
                       tags$li("همیشه پیش‌فرض‌های آزمون‌های پارامتری را بررسی کنید"),
                       tags$li("برای داده‌های غیرنرمال از آزمون‌های ناپارامتری استفاده کنید"),
                       tags$li("اندازه اثر را همراه با p-value گزارش دهید"),
                       tags$li("از تبدیل داده‌ها فقط در صورت توجیه علمی استفاده کنید"),
                       tags$li("حجم نمونه کافی برای تامین توان آماری لازم در نظر بگیرید"),
                       tags$li("نتایج را در context بالینی تفسیر کنید")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۶.۱۶"), "گزارش نتایج"
                 ),
                 div(class = "highlight-box",
                     h4("📝 قالب استاندارد گزارش نتایج"),
                     h5("برای آزمون t:"),
                     p("t(درجه آزادی) = مقدار t, p = مقدار p, d کوهن = اندازه اثر"),
                     p("مثال: t(58) = 2.45, p = 0.017, d = 0.63"),
                     
                     h5("برای آزمون من-ویتنی:"),
                     p("U = مقدار U, p = مقدار p, r = اندازه اثر"),
                     p("مثال: U = 245, p = 0.023, r = 0.32"),
                     
                     h5("تفسیر:"),
                     p("نتایج آزمون t مستقل نشان داد که تفاوت معنی‌داری بین دو گروه وجود دارد (t(58) = 2.45, p = 0.017). اندازه اثر متوسط (d = 0.63) نشان می‌دهد این تفاوت از نظر بالینی نیز قابل توجه است.")
                 )
             )
           ),
           
           "multiple_groups" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۷: آزمون‌هایی برای بیش از دو گروه مستقل"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۱"), "مقدمه"
                 ),
                 div(class = "highlight-box",
                     p("زمانی که می‌خواهیم تفاوت بین میانگین‌ها یا توزیع‌های بیش از دو گروه مستقل را بررسی کنیم، از این آزمون‌ها استفاده می‌کنیم."),
                     tags$ul(
                       tags$li(tags$b("کاربردهای پزشکی:"),
                               tags$ul(
                                 tags$li("مقایسه اثر سه داروی مختلف بر فشار خون"),
                                 tags$li("مقایسه سطح قند خون در چهار گروه سنی مختلف"),
                                 tags$li("مقایسه زمان بهبودی در بیماران با درجات مختلف بیماری")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۲"), "آزمون‌های پارامتری برای چند گروه"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۷.۲.۱"), "آنالیز واریانس یکطرفه (One-Way ANOVA)"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه میانگین سه یا چند گروه مستقل"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: μ₁ = μ₂ = μ₃ = ... (همه میانگین‌ها برابرند)"),
                                 tags$li("H₁: حداقل دو میانگین با هم تفاوت دارند")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمی و پیوسته باشند"),
                                 tags$li("داده‌ها در هر گروه نرمال باشند"),
                                 tags$li("واریانس گروه‌ها برابر باشد (همگنی واریانس)"),
                                 tags$li("مشاهدات مستقل باشند")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "مقایسه میانگین فشار خون در چهار گروه درمانی مختلف")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۷.۲.۲"), "مفاهیم پایه در ANOVA"
                 ),
                 div(class = "info-box",
                     h4("منابع تغییرپذیری در ANOVA"),
                     tags$ul(
                       tags$li(tags$b("تغییرپذیری درون‌گروهی (Within-group):"), "پراکندگی داده‌ها حول میانگین هر گروه"),
                       tags$li(tags$b("تغییرپذیری بین‌گروهی (Between-group):"), "پراکندگی میانگین گروه‌ها حول میانگین کل"),
                       tags$li(tags$b("آماره F:"), "نسبت تغییرپذیری بین‌گروهی به تغییرپذیری درون‌گروهی"),
                       p("F = (بین‌گروهی / درجه آزادی بین‌گروهی) / (درون‌گروهی / درجه آزادی درون‌گروهی)")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۳"), "آزمون‌های ناپارامتری برای چند گروه"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۷.۳.۱"), "آزمون کراسکال-والیس (Kruskal-Wallis)"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه توزیع سه یا چند گروه مستقل"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: توزیع همه گروه‌ها یکسان است"),
                                 tags$li("H₁: حداقل دو گروه توزیع متفاوتی دارند")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمی یا ترتیبی باشند"),
                                 tags$li("نیاز به توزیع نرمال ندارد"),
                                 tags$li("مشاهدات مستقل باشند")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "مقایسه سطح درد در بیماران با سه روش درمانی مختلف")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۴"), "آزمون‌های تعقیبی (Post-hoc Tests)"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۷.۴.۱"), "هدف آزمون‌های تعقیبی"
                 ),
                 div(class = "highlight-box",
                     p("زمانی که ANOVA یا کراسکال-والیس معنی‌دار باشد، از آزمون‌های تعقیبی برای مشخص کردن کدام گروه‌ها با هم تفاوت دارند استفاده می‌کنیم."),
                     
                     h5("برای ANOVA (پارامتری):"),
                     tags$ul(
                       tags$li(tags$b("توكی (Tukey HSD):"), "مقایسه تمام جفت گروه‌ها با کنترل خطای نوع اول"),
                       tags$li(tags$b("شفه (Scheffe):"), "محافظه‌کارانه‌تر، مناسب برای مقایسه‌های برنامه‌ریزی نشده"),
                       tags$li(tags$b("بونفرونی (Bonferroni):"), "اصلاح ساده برای مقایسه‌های چندگانه")
                     ),
                     
                     h5("برای کراسکال-والیس (ناپارامتری):"),
                     tags$ul(
                       tags$li(tags$b("دان (Dunn's test):"), "مقایسه جفت گروه‌ها با اصلاح بونفرونی"),
                       tags$li(tags$b("نم-ونی (Nemenyi test):"), "مشابه توکی برای داده‌های ناپارامتری")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۵"), "راهنمای انتخاب آزمون"
                 ),
                 tableOutput("multi_group_test_guide"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۶"), "شبیه‌ساز ANOVA"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات گروه‌ها"),
                          numericInput("anova_n_groups", "تعداد گروه‌ها:", 
                                       value = 3, min = 3, max = 6),
                          
                          conditionalPanel(
                            condition = "input.anova_n_groups >= 3",
                            h5("میانگین گروه‌ها:"),
                            numericInput("group1_mean", "گروه ۱:", value = 100),
                            numericInput("group2_mean", "گروه ۲:", value = 110),
                            numericInput("group3_mean", "گروه ۳:", value = 105)
                          ),
                          
                          conditionalPanel(
                            condition = "input.anova_n_groups >= 4",
                            numericInput("group4_mean", "گروه ۴:", value = 115)
                          ),
                          
                          conditionalPanel(
                            condition = "input.anova_n_groups >= 5",
                            numericInput("group5_mean", "گروه ۵:", value = 95)
                          ),
                          
                          conditionalPanel(
                            condition = "input.anova_n_groups >= 6",
                            numericInput("group6_mean", "گروه ۶:", value = 120)
                          ),
                          
                          numericInput("anova_sd", "انحراف معیار (یکسان برای همه گروه‌ها):", 
                                       value = 15),
                          numericInput("anova_n_per_group", "تعداد نمونه در هر گروه:", 
                                       value = 30),
                          selectInput("anova_alpha", "سطح معنی‌داری:",
                                      choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                                      selected = "0.05"),
                          actionButton("run_anova", "انجام ANOVA")
                   ),
                   column(8,
                          plotOutput("anova_plot"),
                          verbatimTextOutput("anova_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۷"), "شبیه‌ساز کراسکال-والیس"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات داده‌ها"),
                          numericInput("kw_n_groups", "تعداد گروه‌ها:", 
                                       value = 3, min = 3, max = 6),
                          
                          conditionalPanel(
                            condition = "input.kw_n_groups >= 3",
                            h5("میانگین رتبه گروه‌ها:"),
                            numericInput("kw_group1_rank", "گروه ۱:", value = 25),
                            numericInput("kw_group2_rank", "گروه ۲:", value = 35),
                            numericInput("kw_group3_rank", "گروه ۳:", value = 30)
                          ),
                          
                          conditionalPanel(
                            condition = "input.kw_n_groups >= 4",
                            numericInput("kw_group4_rank", "گروه ۴:", value = 40)
                          ),
                          
                          conditionalPanel(
                            condition = "input.kw_n_groups >= 5",
                            numericInput("kw_group5_rank", "گروه ۵:", value = 20)
                          ),
                          
                          conditionalPanel(
                            condition = "input.kw_n_groups >= 6",
                            numericInput("kw_group6_rank", "گروه ۶:", value = 45)
                          ),
                          
                          numericInput("kw_n_per_group", "تعداد نمونه در هر گروه:", 
                                       value = 30),
                          actionButton("run_kw", "انجام کراسکال-والیس")
                   ),
                   column(8,
                          plotOutput("kw_plot"),
                          verbatimTextOutput("kw_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۸"), "مثال‌های کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("مثال ۱: مقایسه سه روش درمانی"),
                              tags$ul(
                                tags$li(tags$b("سوال:"), "آیا سه روش درمانی A, B, C بر سطح قند خون اثر متفاوتی دارند؟"),
                                tags$li(tags$b("داده‌ها:"), "سطح قند خون بیماران (کمی)"),
                                tags$li(tags$b("توزیع:"), "نرمال"),
                                tags$li(tags$b("آزمون اصلی:"), "ANOVA"),
                                tags$li(tags$b("آزمون تعقیبی:"), "توكی"),
                                tags$li(tags$b("نتایج:"), "p-value = 0.02"),
                                tags$li(tags$b("نتیجه:"), "حداقل دو روش با هم تفاوت دارند")
                              )
                          )
                   ),
                   column(6,
                          div(class = "warning-box",
                              h4("مثال ۲: مقایسه سطح درد در چهار گروه"),
                              tags$ul(
                                tags$li(tags$b("سوال:"), "آیا سطح درد در چهار گروه سنی مختلف تفاوت دارد؟"),
                                tags$li(tags$b("داده‌ها:"), "سطح درد (ترتیبی)"),
                                tags$li(tags$b("توزیع:"), "غیرنرمال"),
                                tags$li(tags$b("آزمون اصلی:"), "کراسکال-والیس"),
                                tags$li(tags$b("آزمون تعقیبی:"), "دان"),
                                tags$li(tags$b("نتایج:"), "p-value = 0.03"),
                                tags$li(tags$b("نتیجه:"), "تفاوت معنی‌دار بین گروه‌ها وجود دارد")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۹"), "اندازه اثر در آزمون‌های چندگروهی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              h5("برای ANOVA"),
                              tags$ul(
                                tags$li(tags$b("اتا مربع (η²):"), "نسبت واریانس تبیین‌شده به واریانس کل"),
                                tags$li(tags$b("تفسیر:"),
                                        tags$ul(
                                          tags$li("η² = 0.01: اثر کوچک"),
                                          tags$li("η² = 0.06: اثر متوسط"),
                                          tags$li("η² = 0.14: اثر بزرگ")
                                        ))
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("برای کراسکال-والیس"),
                              tags$ul(
                                tags$li(tags$b("ε² (اپسیلون مربع):"), "اندازه اثر مبتنی بر رتبه‌ها"),
                                tags$li(tags$b("تفسیر:"),
                                        tags$ul(
                                          tags$li("ε² = 0.01: اثر کوچک"),
                                          tags$li("ε² = 0.08: اثر متوسط"),
                                          tags$li("ε² = 0.26: اثر بزرگ")
                                        ))
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۱۰"), "بررسی پیش‌فرض‌های ANOVA"
                 ),
                 div(class = "warning-box",
                     h4("روش‌های بررسی پیش‌فرض‌ها"),
                     tags$ul(
                       tags$li(tags$b("نرمال بودن:"),
                               tags$ul(
                                 tags$li("آزمون شاپیرو-ویلک برای هر گروه"),
                                 tags$li("نمودار Q-Q برای هر گروه"),
                                 tags$li("ANOVA نسبتاً مقاوم به نقض جزئی نرمال بودن است")
                               )),
                       tags$li(tags$b("همگنی واریانس:"),
                               tags$ul(
                                 tags$li("آزمون لوین (Levene's test)"),
                                 tags$li("آزمون بارتلت (Bartlett's test)"),
                                 tags$li("نمودار جعبه‌ای برای بررسی بصری")
                               )),
                       tags$li(tags$b("اقدامات در صورت نقض پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("تبدیل داده‌ها (لگاریتمی، جذر)"),
                                 tags$li("استفاده از آزمون ناپارامتری (کراسکال-والیس)"),
                                 tags$li("استفاده از ANOVA با اصلاح ویلچ (Welch)")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۱۱"), "خلاصه آزمون‌های چندگروهی"
                 ),
                 tableOutput("multi_group_summary_table"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۱۲"), "نکات عملی برای پژوهشگران"
                 ),
                 div(class = "highlight-box",
                     h4("توصیه‌های مهم"),
                     tags$ul(
                       tags$li("همیشه پیش‌فرض‌های ANOVA را بررسی کنید"),
                       tags$li("اگر ANOVA معنی‌دار شد، حتماً از آزمون تعقیبی استفاده کنید"),
                       tags$li("برای داده‌های غیرنرمال از کراسکال-والیس استفاده کنید"),
                       tags$li("اندازه اثر را همراه با p-value گزارش دهید"),
                       tags$li("در گزارش نتایج، مقادیر دقیق p-value را ذکر کنید"),
                       tags$li("از نمودارهای مناسب برای نمایش تفاوت گروه‌ها استفاده کنید")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۷.۱۳"), "گزارش نتایج"
                 ),
                 div(class = "info-box",
                     h4("قالب استاندارد گزارش نتایج"),
                     h5("برای ANOVA:"),
                     p("F(درجه آزادی بین, درجه آزادی درون) = مقدار F, p = مقدار p, η² = اندازه اثر"),
                     p("مثال: F(2, 87) = 5.43, p = 0.006, η² = 0.11"),
                     
                     h5("برای کراسکال-والیس:"),
                     p("H(درجه آزادی) = مقدار H, p = مقدار p, ε² = اندازه اثر"),
                     p("مثال: H(2) = 8.76, p = 0.013, ε² = 0.15"),
                     
                     h5("برای آزمون‌های تعقیبی:"),
                     p("نتایج آزمون توکی نشان داد که گروه A به طور معنی‌داری از گروه B متفاوت است (p = 0.02)، اما تفاوت بین گروه A و C معنی‌دار نبود (p = 0.45).")
                 )
             )
           ),
           
           "categorical_tests" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۸: آزمون‌های آماری برای متغیرهای کیفی"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۱"), "مقدمه"
                 ),
                 div(class = "highlight-box",
                     p("این آزمون‌ها برای بررسی رابطه بین متغیرهای کیفی (اسمی و ترتیبی) استفاده می‌شوند. زمانی که داده‌های ما به صورت دسته‌ای هستند و نمی‌توان از آزمون‌های پارامتری استفاده کرد، از این آزمون‌ها بهره می‌گیریم."),
                     tags$ul(
                       tags$li(tags$b("متغیرهای کیفی اسمی:"), "گروه خونی، جنسیت، نوع بیماری"),
                       tags$li(tags$b("متغیرهای کیفی ترتیبی:"), "درجه سرطان، سطح درد، میزان رضایت"),
                       tags$li(tags$b("کاربردهای پزشکی:"),
                               tags$ul(
                                 tags$li("بررسی رابطه بین جنسیت و نوع بیماری"),
                                 tags$li("مقایسه اثر بخشی درمان‌ها در گروه‌های مختلف"),
                                 tags$li("بررسی ارتباط عوامل خطر با بروز بیماری")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۲"), "آزمون کای-دو (Chi-Square Test)"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۸.۲.۱"), "آزمون کای-دو برای استقلال"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "بررسی استقلال یا رابطه بین دو متغیر کیفی"),
                       tags$li(tags$b("فرضیه‌ها:"),
                               tags$ul(
                                 tags$li("H₀: دو متغیر مستقل هستند (ارتباطی ندارند)"),
                                 tags$li("H₁: دو متغیر وابسته هستند (ارتباط دارند)")
                               )),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها در جدول توافقی قرار گیرند"),
                                 tags$li("مشاهدات مستقل باشند"),
                                 tags$li("حداقل ۸۰٪ خانه‌ها فراوانی مورد انتظار ≥ ۵ داشته باشند"),
                                 tags$li("هیچ خانه‌ای فراوانی مورد انتظار صفر نداشته باشد")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "بررسی رابطه بین سیگار کشیدن و سرطان ریه")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۳"), "آزمون فیشر (Fisher's Exact Test)"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۸.۳.۱"), "کاربرد آزمون فیشر"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "بررسی استقلال دو متغیر کیفی در نمونه‌های کوچک"),
                       tags$li(tags$b("موارد استفاده:"),
                               tags$ul(
                                 tags$li("حجم نمونه کوچک (n < 20)"),
                                 tags$li("فراوانی مورد انتظار در برخی خانه‌ها کمتر از ۵ باشد"),
                                 tags$li("جدول ۲×۲"),
                                 tags$li("داده‌های بسیار نادر یا sparse")
                               )),
                       tags$li(tags$b("مزایا:"),
                               tags$ul(
                                 tags$li("نیاز به پیش‌فرض‌های کمتر"),
                                 tags$li("دقت بالا در نمونه‌های کوچک"),
                                 tags$li("مناسب برای داده‌های نادر")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), "بررسی رابطه بین یک بیماری نادر و یک عامل خطر")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۴"), "آزمون نسبت (Proportion Test)"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۸.۴.۱"), "آزمون‌های برای مقایسه نسبت‌ها"
                 ),
                 div(class = "info-box",
                     tags$ul(
                       tags$li(tags$b("آزمون نسبت یک نمونه‌ای:"), 
                               "مقایسه نسبت مشاهده شده با یک مقدار فرضی"),
                       tags$li(tags$b("آزمون نسبت دو نمونه‌ای:"), 
                               "مقایسه دو نسبت مستقل از هم"),
                       tags$li(tags$b("آزمون نسبت چند نمونه‌ای:"), 
                               "مقایسه چند نسبت با هم"),
                       tags$li(tags$b("مثال پزشکی:"), 
                               "مقایسه نرخ بهبودی در دو روش درمانی مختلف")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۵"), "آزمون مک نمار (McNemar's Test)"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۸.۵.۱"), "آزمون مک نمار برای داده‌های وابسته"
                 ),
                 div(class = "highlight-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), 
                               "مقایسه دو اندازه‌گیری مکرر روی یک نمونه"),
                       tags$li(tags$b("کاربرد:"),
                               tags$ul(
                                 tags$li("قبل و بعد از درمان"),
                                 tags$li("دو روش اندازه‌گیری روی یک نمونه"),
                                 tags$li("داده‌های جفت‌شده کیفی")
                               )),
                       tags$li(tags$b("مثال پزشکی:"), 
                               "بررسی تغییر وضعیت بیماران قبل و بعد از درمان")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۶"), "راهنمای انتخاب آزمون"
                 ),
                 tableOutput("categorical_test_selection_guide"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۷"), "شبیه‌ساز آزمون کای-دو"
                 ),
                 fluidRow(
                   column(4,
                          h4("داده‌های جدول توافقی"),
                          numericInput("chi2_row1_col1", "ردیف ۱ - ستون ۱:", value = 30),
                          numericInput("chi2_row1_col2", "ردیف ۱ - ستون ۲:", value = 20),
                          numericInput("chi2_row2_col1", "ردیف ۲ - ستون ۱:", value = 25),
                          numericInput("chi2_row2_col2", "ردیف ۲ - ستون ۲:", value = 25),
                          selectInput("chi2_alpha", "سطح معنی‌داری:",
                                      choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                                      selected = "0.05"),
                          actionButton("run_chi2", "انجام آزمون کای-دو")
                   ),
                   column(8,
                          plotOutput("chi2_plot"),
                          verbatimTextOutput("chi2_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۸"), "شبیه‌ساز آزمون فیشر"
                 ),
                 fluidRow(
                   column(4,
                          h4("داده‌های جدول ۲×۲"),
                          numericInput("fisher_row1_col1", "ردیف ۱ - ستون ۱:", value = 8),
                          numericInput("fisher_row1_col2", "ردیف ۱ - ستون ۲:", value = 2),
                          numericInput("fisher_row2_col1", "ردیف ۲ - ستون ۱:", value = 1),
                          numericInput("fisher_row2_col2", "ردیف ۲ - ستون ۲:", value = 9),
                          actionButton("run_fisher", "انجام آزمون فیشر")
                   ),
                   column(8,
                          plotOutput("fisher_plot"),
                          verbatimTextOutput("fisher_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۹"), "مثال‌های کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("مثال ۱: رابطه سیگار و سرطان ریه"),
                              tags$ul(
                                tags$li(tags$b("متغیرها:"), "سیگار (بلی/خیر) و سرطان ریه (دارد/ندارد)"),
                                tags$li(tags$b("داده‌ها:"), "جدول ۲×۲ با حجم نمونه بزرگ"),
                                tags$li(tags$b("آزمون:"), "کای-دو"),
                                tags$li(tags$b("نتایج:"), "χ² = 15.8, p < 0.001"),
                                tags$li(tags$b("نتیجه:"), "رابطه معنی‌دار بین سیگار و سرطان ریه وجود دارد")
                              )
                          )
                   ),
                   column(6,
                          div(class = "warning-box",
                              h4("مثال ۲: اثر دارو بر بیماری نادر"),
                              tags$ul(
                                tags$li(tags$b("متغیرها:"), "دارو (جدید/قدیم) و بهبودی (بلی/خیر)"),
                                tags$li(tags$b("داده‌ها:"), "جدول ۲×۲ با فراوانی کم"),
                                tags$li(tags$b("آزمون:"), "فیشر"),
                                tags$li(tags$b("نتایج:"), "p = 0.045"),
                                tags$li(tags$b("نتیجه:"), "داروی جدید اثر معنی‌داری دارد")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۱۰"), "اندازه اثر در آزمون‌های کیفی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              h5("برای جدول ۲×۲"),
                              tags$ul(
                                tags$li(tags$b("ضریب فی (φ):"), "برای جدول ۲×۲"),
                                tags$li(tags$b("کرامرز V:"), "برای جدول بزرگتر"),
                                tags$li(tags$b("تفسیر:"),
                                        tags$ul(
                                          tags$li("0.1 > : اثر کوچک"),
                                          tags$li("0.3 > : اثر متوسط"),
                                          tags$li("0.5 > : اثر بزرگ")
                                        ))
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("نسبت شانس (Odds Ratio)"),
                              tags$ul(
                                tags$li("OR = 1: عدم ارتباط"),
                                tags$li("OR > 1: ارتباط مثبت"),
                                tags$li("OR < 1: ارتباط منفی"),
                                tags$li("فاصله اطمینان شامل ۱ نباشد: معنی‌دار")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۱۱"), "بررسی پیش‌فرض‌های آزمون کای-دو"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۸.۱۱.۱"), "روش‌های بررسی پیش‌فرض‌ها"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("فراوانی مورد انتظار:"),
                               tags$ul(
                                 tags$li("محاسبه فراوانی مورد انتظار برای هر خانه"),
                                 tags$li("فرمول: (مجموع سطر × مجموع ستون) / کل"),
                                 tags$li("حداقل ۸۰٪ خانه‌ها باید ≥ ۵ باشند"),
                                 tags$li("هیچ خانه‌ای صفر نباشد")
                               )),
                       tags$li(tags$b("اقدامات در صورت نقض پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("ادغام دسته‌ها (در صورت معنی‌دار بودن)"),
                                 tags$li("استفاده از آزمون فیشر"),
                                 tags$li("استفاده از شبیه‌سازی مونت کارلو")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۱۲"), "شبیه‌ساز بررسی پیش‌فرض کای-دو"
                 ),
                 fluidRow(
                   column(4,
                          h4("داده‌های جدول"),
                          numericInput("assumption_row1_col1", "ردیف ۱ - ستون ۱:", value = 15),
                          numericInput("assumption_row1_col2", "ردیف ۱ - ستون ۲:", value = 10),
                          numericInput("assumption_row2_col1", "ردیف ۲ - ستون ۱:", value = 5),
                          numericInput("assumption_row2_col2", "ردیف ۲ - ستون ۲:", value = 20),
                          actionButton("check_assumptions", "بررسی پیش‌فرض‌ها")
                   ),
                   column(8,
                          plotOutput("assumption_plot"),
                          verbatimTextOutput("assumption_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۱۳"), "آزمون‌های برای متغیرهای ترتیبی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۸.۱۳.۱"), "آزمون‌های ویژه داده‌های ترتیبی"
                 ),
                 div(class = "info-box",
                     tags$ul(
                       tags$li(tags$b("آزمون ترند کاکران-آرمیتاژ:"), 
                               "بررسی روند خطی در داده‌های ترتیبی"),
                       tags$li(tags$b("آزمون من-ویتنی:"), 
                               "مقایسه دو گروه مستقل با داده‌های ترتیبی"),
                       tags$li(tags$b("آزمون کراسکال-والیس:"), 
                               "مقایسه چند گروه مستقل با داده‌های ترتیبی"),
                       tags$li(tags$b("آزمون ویلکاکسون:"), 
                               "مقایسه دو اندازه‌گیری وابسته با داده‌های ترتیبی")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۱۴"), "نکات عملی برای پژوهشگران"
                 ),
                 div(class = "highlight-box",
                     h4("توصیه‌های مهم"),
                     tags$ul(
                       tags$li("همیشه فراوانی مورد انتظار را بررسی کنید"),
                       tags$li("برای نمونه‌های کوچک از آزمون فیشر استفاده کنید"),
                       tags$li("اندازه اثر را همراه با p-value گزارش دهید"),
                       tags$li("در صورت امکان، نسبت شانس و فاصله اطمینان آن را گزارش دهید"),
                       tags$li("برای داده‌های ترتیبی از آزمون‌های مناسب استفاده کنید"),
                       tags$li("از ادغام دسته‌ها بدون توجیه علمی خودداری کنید")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۸.۱۵"), "گزارش نتایج"
                 ),
                 div(class = "success-box",
                     h4("قالب استاندارد گزارش نتایج"),
                     h5("برای آزمون کای-دو:"),
                     p("χ²(درجه آزادی, N = حجم نمونه) = مقدار کای-دو, p = مقدار p, φ/V = اندازه اثر"),
                     p("مثال: χ²(1, N = 100) = 15.8, p < 0.001, φ = 0.40"),
                     
                     h5("برای آزمون فیشر:"),
                     p("آزمون فیشر: p = مقدار p, OR = نسبت شانس"),
                     p("مثال: p = 0.045, OR = 4.5"),
                     
                     h5("تفسیر:"),
                     p("نتایج نشان داد رابطه معنی‌داری بین سیگار کشیدن و سرطان ریه وجود دارد (χ² = 15.8, p < 0.001). اندازه اثر متوسط (φ = 0.40) نشان می‌دهد این رابطه از نظر بالینی نیز قابل توجه است.")
                 )
             )
           ),
           
           "ancova" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۹: آنالیز کوواریانس (ANCOVA)"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۹.۱"), "مقدمه"
                 ),
                 div(class = "highlight-box",
                     h4("تعریف ANCOVA"),
                     p("آنالیز کوواریانس (ANCOVA) ترکیبی از ANOVA و رگرسیون است که برای کنترل اثر متغیرهای کمّی مزاحم (کوواریات) هنگام مقایسه گروه‌ها استفاده می‌شود."),
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه میانگین گروه‌ها پس از کنترل اثر متغیرهای کمّی"),
                       tags$li(tags$b("کاربردهای پزشکی:"),
                               tags$ul(
                                 tags$li("مقایسه اثر درمان‌ها پس از کنترل سن بیماران"),
                                 tags$li("بررسی تفاوت گروه‌ها پس از کنترل پیش‌آزمون"),
                                 tags$li("حذف اثر متغیرهای مخدوشگر")
                               )),
                       tags$li(tags$b("مزایا:"),
                               tags$ul(
                                 tags$li("افزایش دقت مقایسه‌ها"),
                                 tags$li("کاهش واریانس خطا"),
                                 tags$li("کنترل متغیرهای مزاحم")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۹.۲"), "مفاهیم پایه در ANCOVA"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۹.۲.۱"), "اجزای اصلی ANCOVA"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li(tags$b("متغیر وابسته:"), "متغیر کمّی که می‌خواهیم مقایسه کنیم"),
                       tags$li(tags$b("متغیر مستقل:"), "متغیر گروه‌بندی (کیفی)"),
                       tags$li(tags$b("کوواریات:"), "متغیر کمّی که می‌خواهیم اثر آن را کنترل کنیم"),
                       tags$li(tags$b("مدل:"), "Y = μ + αᵢ + βX + ε"),
                       tags$li(tags$b("مثال:"), "مقایسه فشار خون پس از درمان، پس از کنترل فشار خون پایه")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۹.۳"), "پیش‌فرض‌های ANCOVA"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۹.۳.۱"), "پیش‌فرض‌های مهم"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("خطی بودن:"), "رابطه خطی بین کوواریات و متغیر وابسته"),
                       tags$li(tags$b("همگونی شیب‌های رگرسیون:"), "شیب رابطه در همه گروه‌ها یکسان باشد"),
                       tags$li(tags$b("نرمال بودن باقیمانده‌ها:"), "باقیمانده‌ها توزیع نرمال داشته باشند"),
                       tags$li(tags$b("همسانی واریانس‌ها:"), "واریانس خطا در گروه‌ها یکسان باشد"),
                       tags$li(tags$b("همسانی واریانس-کوواریانس:"), "برای چندین کوواریات")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۹.۴"), "بررسی پیش‌فرض همگونی شیب‌ها"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۹.۴.۱"), "آزمون برهمکنش گروه × کوواریات"
                 ),
                 div(class = "info-box",
                     tags$ul(
                       tags$li("اگر برهمکنش معنی‌دار نباشد: پیش‌فرض برقرار است"),
                       tags$li("اگر برهمکنش معنی‌دار باشد: پیش‌فرض نقض شده است"),
                       tags$li("راه‌حل: استفاده از مدل‌های پیچیده‌تر یا گزارش جداگانه")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۹.۵"), "شبیه‌ساز ANCOVA"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات داده‌ها"),
                          numericInput("ancova_n_groups", "تعداد گروه‌ها:", 
                                       value = 2, min = 2, max = 4),
                          numericInput("ancova_n_per_group", "تعداد نمونه در هر گروه:", 
                                       value = 30),
                          
                          h5("میانگین متغیر وابسته در گروه‌ها:"),
                          numericInput("ancova_group1_mean", "گروه ۱:", value = 50),
                          numericInput("ancova_group2_mean", "گروه ۲:", value = 55),
                          conditionalPanel(
                            condition = "input.ancova_n_groups >= 3",
                            numericInput("ancova_group3_mean", "گروه ۳:", value = 52)
                          ),
                          conditionalPanel(
                            condition = "input.ancova_n_groups >= 4",
                            numericInput("ancova_group4_mean", "گروه ۴:", value = 58)
                          ),
                          
                          h5("کوواریات:"),
                          sliderInput("ancova_cov_effect", "اثر کوواریات:", 
                                      min = 0, max = 2, value = 0.5, step = 0.1),
                          sliderInput("ancova_cov_correlation", "همبستگی کوواریات با متغیر وابسته:", 
                                      min = 0, max = 0.8, value = 0.3, step = 0.1),
                          
                          actionButton("run_ancova", "انجام ANCOVA")
                   ),
                   column(8,
                          plotOutput("ancova_plot"),
                          verbatimTextOutput("ancova_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۹.۶"), "مثال کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("مثال ۱: مقایسه درمان‌ها با کنترل سن"),
                              tags$ul(
                                tags$li(tags$b("متغیر وابسته:"), "فشار خون پس از درمان"),
                                tags$li(tags$b("متغیر مستقل:"), "نوع درمان (۳ گروه)"),
                                tags$li(tags$b("کوواریات:"), "سن بیمار"),
                                tags$li(tags$b("نتایج:"), "پس از کنترل سن، تفاوت معنی‌دار بین درمان‌ها وجود دارد")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("مثال ۲: مطالعه آموزشی با کنترل پیش‌آزمون"),
                              tags$ul(
                                tags$li(tags$b("متغیر وابسته:"), "نمره پس‌آزمون"),
                                tags$li(tags$b("متغیر مستقل:"), "روش آموزشی (۲ گروه)"),
                                tags$li(tags$b("کوواریات:"), "نمره پیش‌آزمون"),
                                tags$li(tags$b("نتایج:"), "پس از کنترل پیش‌آزمون، روش جدید بهتر است")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۹.۷"), "نکات مهم و هشدارها"
                 ),
                 div(class = "warning-box",
                     h4("اشتباهات رایج"),
                     tags$ul(
                       tags$li("استفاده از کوواریات‌های بسیار همبسته با متغیر مستقل"),
                       tags$li("بی‌توجهی به پیش‌فرض همگونی شیب‌ها"),
                       tags$li("استفاده از کوواریات‌های کیفی"),
                       tags$li("تفسیر نادرست میانگین‌های تعدیل‌شده"),
                       tags$li("گزارش نکردن بررسی پیش‌فرض‌ها")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۹.۸"), "گزارش نتایج ANCOVA"
                 ),
                 div(class = "highlight-box",
                     h4("قالب استاندارد گزارش"),
                     p("پس از کنترل اثر [نام کوواریات]، تفاوت معنی‌داری بین گروه‌ها مشاهده شد (F(درجه آزادی بین, درجه آزادی درون) = مقدار F, p = مقدار p, η² = اندازه اثر)."),
                     p("میانگین‌های تعدیل‌شده: گروه ۱ = مقدار, گروه ۲ = مقدار, ..."),
                     p("بررسی پیش‌فرض همگونی شیب‌ها معنی‌دار نبود (p > 0.05).")
                 )
             )
           ),
           
           "repeated_measures" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۱۰: مقایسه بیش از دو گروه وابسته"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۱"), "مقدمه"
                 ),
                 div(class = "highlight-box",
                     p("زمانی که اندازه‌گیری‌های متعددی از یک نمونه در زمان‌ها یا شرایط مختلف انجام می‌شود، از این روش‌ها استفاده می‌کنیم."),
                     tags$ul(
                       tags$li(tags$b("کاربردهای پزشکی:"),
                               tags$ul(
                                 tags$li("اندازه‌گیری فشار خون در زمان‌های مختلف"),
                                 tags$li("بررسی اثر دارو در دوزهای مختلف"),
                                 tags$li("مطالعات طولی و پیگیری بیماران")
                               )),
                       tags$li(tags$b("مزایا:"),
                               tags$ul(
                                 tags$li("کنترل تفاوت‌های فردی"),
                                 tags$li("نیاز به نمونه کمتر"),
                                 tags$li("قدرت آماری بالاتر")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۲"), "تحلیل واریانس با اندازه‌گیری مکرر"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۰.۲.۱"), "ویژگی‌های Repeated Measures ANOVA"
                 ),
                 div(class = "success-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه میانگین‌های چند اندازه‌گیری وابسته"),
                       tags$li(tags$b("پیش‌فرض‌ها:"),
                               tags$ul(
                                 tags$li("داده‌ها کمّی و نرمال باشند"),
                                 tags$li("کروی بودن (Sphericity) برقرار باشد"),
                                 tags$li("مشاهدات مستقل بین واحدها باشند")
                               )),
                       tags$li(tags$b("مثال:"), "مقایسه فشار خون بیماران در ۴ نوبت مختلف")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۳"), "آزمون فریدمن (Friedman Test)"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۰.۳.۱"), "آزمون ناپارامتری برای داده‌های وابسته"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه توزیع چند اندازه‌گیری وابسته"),
                       tags$li(tags$b("کاربرد:"),
                               tags$ul(
                                 tags$li("داده‌های ترتیبی"),
                                 tags$li("داده‌های کمّی غیرنرمال"),
                                 tags$li("نمونه‌های کوچک")
                               )),
                       tags$li(tags$b("مثال:"), "مقایسه سطح درد در ۳ روش درمانی مختلف")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۴"), "آزمون Q کوکران (Cochran's Q Test)"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۰.۴.۱"), "آزمون برای داده‌های دوحالتی وابسته"
                 ),
                 div(class = "info-box",
                     tags$ul(
                       tags$li(tags$b("هدف:"), "مقایسه نسبت‌های چند اندازه‌گیری وابسته"),
                       tags$li(tags$b("کاربرد:"),
                               tags$ul(
                                 tags$li("داده‌های دوحالتی (بله/خیر)"),
                                 tags$li("اندازه‌گیری‌های مکرر"),
                                 tags$li("متغیرهای اسمی")
                               )),
                       tags$li(tags$b("مثال:"), "مقایسه نرخ بهبودی در ۴ مرحله درمان")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۵"), "پیش‌فرض کروی بودن (Sphericity)"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۰.۵.۱"), "بررسی و اصلاح کروی بودن"
                 ),
                 div(class = "warning-box",
                     tags$ul(
                       tags$li(tags$b("آزمون ماشلی (Mauchly's Test):"), "برای بررسی کروی بودن"),
                       tags$li(tags$b("اصلاح‌ها در صورت نقض:"),
                               tags$ul(
                                 tags$li("گرین هاوس-گیسر (Greenhouse-Geisser)"),
                                 tags$li("هاینه-فلدت (Huynh-Feldt)"),
                                 tags$li("پایین‌ترین حد (Lower-bound)")
                               )),
                       tags$li(tags$b("اقدام:"), "اگر p-value ماشلی < 0.05، از اصلاح استفاده کنید")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۶"), "شبیه‌ساز Repeated Measures ANOVA"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات مطالعه"),
                          numericInput("rm_n_subjects", "تعداد واحدهای مشاهده:", 
                                       value = 20, min = 5, max = 100),
                          numericInput("rm_n_timepoints", "تعداد زمان‌های اندازه‌گیری:", 
                                       value = 3, min = 2, max = 6),
                          
                          h5("میانگین‌ها در زمان‌های مختلف:"),
                          numericInput("rm_time1_mean", "زمان ۱:", value = 50),
                          numericInput("rm_time2_mean", "زمان ۲:", value = 45),
                          numericInput("rm_time3_mean", "زمان ۳:", value = 40),
                          conditionalPanel(
                            condition = "input.rm_n_timepoints >= 4",
                            numericInput("rm_time4_mean", "زمان ۴:", value = 38)
                          ),
                          conditionalPanel(
                            condition = "input.rm_n_timepoints >= 5",
                            numericInput("rm_time5_mean", "زمان ۵:", value = 37)
                          ),
                          conditionalPanel(
                            condition = "input.rm_n_timepoints >= 6",
                            numericInput("rm_time6_mean", "زمان ۶:", value = 36)
                          ),
                          
                          sliderInput("rm_correlation", "همبستگی بین اندازه‌گیری‌ها:", 
                                      min = 0.1, max = 0.9, value = 0.5, step = 0.1),
                          actionButton("run_rm_anova", "انجام تحلیل")
                   ),
                   column(8,
                          plotOutput("rm_plot"),
                          verbatimTextOutput("rm_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۷"), "شبیه‌ساز آزمون فریدمن"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات داده‌های رتبه‌ای"),
                          numericInput("friedman_n_blocks", "تعداد بلوک‌ها (موضوعات):", 
                                       value = 15, min = 5, max = 50),
                          numericInput("friedman_n_treatments", "تعداد درمان‌ها:", 
                                       value = 3, min = 2, max = 6),
                          
                          h5("میانگین رتبه درمان‌ها:"),
                          numericInput("friedman_treatment1", "درمان ۱:", value = 1.5),
                          numericInput("friedman_treatment2", "درمان ۲:", value = 2.0),
                          numericInput("friedman_treatment3", "درمان ۳:", value = 2.5),
                          conditionalPanel(
                            condition = "input.friedman_n_treatments >= 4",
                            numericInput("friedman_treatment4", "درمان ۴:", value = 1.8)
                          ),
                          conditionalPanel(
                            condition = "input.friedman_n_treatments >= 5",
                            numericInput("friedman_treatment5", "درمان ۵:", value = 2.2)
                          ),
                          conditionalPanel(
                            condition = "input.friedman_n_treatments >= 6",
                            numericInput("friedman_treatment6", "درمان ۶:", value = 2.8)
                          ),
                          
                          actionButton("run_friedman", "انجام آزمون فریدمن")
                   ),
                   column(8,
                          plotOutput("friedman_plot"),
                          verbatimTextOutput("friedman_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۸"), "شبیه‌ساز آزمون Q کوکران"
                 ),
                 fluidRow(
                   column(4,
                          h4("داده‌های دوحالتی"),
                          numericInput("cochran_n_subjects", "تعداد بیماران:", 
                                       value = 20, min = 5, max = 50),
                          numericInput("cochran_n_treatments", "تعداد درمان‌ها:", 
                                       value = 3, min = 2, max = 5),
                          
                          h5("نسبت بهبودی در هر درمان (%):"),
                          sliderInput("cochran_treatment1", "درمان ۱:", 
                                      min = 0, max = 100, value = 30),
                          sliderInput("cochran_treatment2", "درمان ۲:", 
                                      min = 0, max = 100, value = 50),
                          sliderInput("cochran_treatment3", "درمان ۳:", 
                                      min = 0, max = 100, value = 70),
                          conditionalPanel(
                            condition = "input.cochran_n_treatments >= 4",
                            sliderInput("cochran_treatment4", "درمان ۴:", 
                                        min = 0, max = 100, value = 60)
                          ),
                          conditionalPanel(
                            condition = "input.cochran_n_treatments >= 5",
                            sliderInput("cochran_treatment5", "درمان ۵:", 
                                        min = 0, max = 100, value = 40)
                          ),
                          
                          actionButton("run_cochran", "انجام آزمون کوکران")
                   ),
                   column(8,
                          plotOutput("cochran_plot"),
                          verbatimTextOutput("cochran_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۹"), "مثال‌های کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(4,
                          div(class = "success-box",
                              h4("مثال ۱: مطالعه فشار خون"),
                              tags$ul(
                                tags$li(tags$b("طرح:"), "اندازه‌گیری مکرر"),
                                tags$li(tags$b("داده‌ها:"), "فشار خون در ۴ نوبت"),
                                tags$li(tags$b("آزمون:"), "Repeated Measures ANOVA"),
                                tags$li(tags$b("نتیجه:"), "تفاوت معنی‌دار بین زمان‌ها")
                              )
                          )
                   ),
                   column(4,
                          div(class = "info-box",
                              h4("مثال ۲: مقایسه روش‌های کاهش درد"),
                              tags$ul(
                                tags$li(tags$b("طرح:"), "داده‌های ترتیبی"),
                                tags$li(tags$b("داده‌ها:"), "سطح درد با ۳ روش"),
                                tags$li(tags$b("آزمون:"), "فریدمن"),
                                tags$li(tags$b("نتیجه:"), "تفاوت در توزیع درد")
                              )
                          )
                   ),
                   column(4,
                          div(class = "warning-box",
                              h4("مثال ۳: مطالعه بهبودی بیماران"),
                              tags$ul(
                                tags$li(tags$b("طرح:"), "داده‌های دوحالتی"),
                                tags$li(tags$b("داده‌ها:"), "بهبودی/عدم بهبودی در ۳ مرحله"),
                                tags$li(tags$b("آزمون:"), "کوکران"),
                                tags$li(tags$b("نتیجه:"), "تفاوت در نرخ بهبودی")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۱۰"), "خلاصه آزمون‌ها"
                 ),
                 tableOutput("repeated_measures_summary_table"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۰.۱۱"), "نکات عملی"
                 ),
                 div(class = "highlight-box",
                     h4("توصیه‌های مهم"),
                     tags$ul(
                       tags$li("برای داده‌های نرمال از Repeated Measures ANOVA استفاده کنید"),
                       tags$li("برای داده‌های غیرنرمال از فریدمن استفاده کنید"),
                       tags$li("برای داده‌های دوحالتی از کوکران استفاده کنید"),
                       tags$li("همیشه پیش‌فرض کروی بودن را بررسی کنید"),
                       tags$li("از آزمون‌های تعقیبی مناسب استفاده کنید"),
                       tags$li("حجم نمونه کافی در نظر بگیرید")
                     )
                 )
             )
           ),
           
           # در بخش UI، محتوای فصل ۱۱ به بعد را به این صورت اضافه کنید:
           
           # در تابع renderUI در سرور، بخش‌های مربوط به فصل ۱۱ به بعد را اضافه می‌کنیم:
           
           "correlation" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۱۱: همبستگی"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۱"), "مقدمه همبستگی"
                 ),
                 div(class = "highlight-box",
                     h4("تعریف همبستگی"),
                     p("همبستگی اندازه‌گیری رابطه خطی بین دو متغیر کمی است. همبستگی نشان می‌دهد که چگونه تغییرات یک متغیر با تغییرات متغیر دیگر مرتبط است."),
                     tags$ul(
                       tags$li(tags$b("ضریب همبستگی:"), "عدد بین ۱- و ۱+ که قدرت و جهت رابطه را نشان می‌دهد"),
                       tags$li(tags$b("جهت رابطه:"), "مثبت (هم‌جهت) یا منفی (خلاف جهت)"),
                       tags$li(tags$b("مثال‌های پزشکی:"),
                               tags$ul(
                                 tags$li("رابطه سن و فشار خون"),
                                 tags$li("رابطه وزن و سطح کلسترول"),
                                 tags$li("رابطه دوز دارو و پاسخ درمانی")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۲"), "انواع همبستگی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۱.۲.۱"), "همبستگی‌های پارامتری"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("همبستگی پیرسون"),
                              tags$ul(
                                tags$li(tags$b("ضریب همبستگی پیرسون:"),
                                        tags$ul(
                                          tags$li("برای داده‌های کمی نرمال"),
                                          tags$li("رابطه خطی بین دو متغیر"),
                                          tags$li("مقادیر بین ۱- و ۱+")
                                        )),
                                tags$li(tags$b("پیش‌فرض‌های پیرسون:"),
                                        tags$ul(
                                          tags$li("داده‌ها کمی و پیوسته"),
                                          tags$li("توزیع نرمال دو متغیره"),
                                          tags$li("رابطه خطی"),
                                          tags$li("همگنی واریانس")
                                        ))
                              )
                          )
                   ),
                   column(6,
                          div(class = "warning-box",
                              h4("همبستگی‌های ناپارامتری"),
                              tags$ul(
                                tags$li(tags$b("ضریب همبستگی اسپیرمن:"),
                                        tags$ul(
                                          tags$li("برای داده‌های ترتیبی یا غیرنرمال"),
                                          tags$li("بر اساس رتبه‌ها"),
                                          tags$li("نیاز به توزیع نرمال ندارد")
                                        )),
                                tags$li(tags$b("ضریب همبستگی کندال:"),
                                        tags$ul(
                                          tags$li("برای داده‌های ترتیبی"),
                                          tags$li("مقاوم به مقادیر پرت"),
                                          tags$li("تفسیر بر اساس احتمال هماهنگی")
                                        ))
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۳"), "تفسیر ضریب همبستگی"
                 ),
                 div(class = "info-box",
                     h4("مقیاس تفسیر همبستگی"),
                     tableOutput("correlation_interpretation_table"),
                     h5("نکات مهم:"),
                     tags$ul(
                       tags$li("همبستگی ≠ علیت (Correlation ≠ Causation)"),
                       tags$li("همبستگی قوی لزوماً به معنای رابطه علی نیست"),
                       tags$li("همبستگی می‌تواند تحت تأثیر متغیرهای سوم باشد")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۴"), "نمودارهای همبستگی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              h5("نمودار پراکندگی (Scatter Plot)"),
                              p("برای نمایش بصری رابطه بین دو متغیر"),
                              tags$ul(
                                tags$li("محور X: متغیر مستقل"),
                                tags$li("محور Y: متغیر وابسته"),
                                tags$li("خط روند: نشان‌دهنده جهت رابطه")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("ماتریس همبستگی (Correlation Matrix)"),
                              p("برای نمایش همبستگی بین چندین متغیر"),
                              tags$ul(
                                tags$li("مناسب برای مطالعات اکتشافی"),
                                tags$li("شناسایی الگوهای همبستگی"),
                                tags$li("تشخیص همخطی (Multicollinearity)")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۵"), "شبیه‌ساز همبستگی پیرسون"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات داده‌ها"),
                          sliderInput("pearson_cor", "ضریب همبستگی مورد نظر:", 
                                      min = -1, max = 1, value = 0.7, step = 0.1),
                          numericInput("cor_sample_size", "حجم نمونه:", 
                                       value = 100, min = 10, max = 1000),
                          sliderInput("cor_noise", "درجه نویز داده‌ها:", 
                                      min = 0, max = 2, value = 0.5, step = 0.1),
                          selectInput("cor_alpha", "سطح معنی‌داری:",
                                      choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                                      selected = "0.05"),
                          actionButton("run_pearson", "محاسبه همبستگی پیرسون")
                   ),
                   column(8,
                          plotOutput("pearson_plot"),
                          verbatimTextOutput("pearson_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۶"), "شبیه‌ساز همبستگی اسپیرمن"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات داده‌ها"),
                          sliderInput("spearman_cor", "ضریب همبستگی مورد نظر:", 
                                      min = -1, max = 1, value = 0.6, step = 0.1),
                          numericInput("spearman_sample_size", "حجم نمونه:", 
                                       value = 100, min = 10, max = 1000),
                          sliderInput("spearman_outliers", "تعداد مقادیر پرت:", 
                                      min = 0, max = 10, value = 2),
                          checkboxInput("use_nonlinear", "رابطه غیرخطی", value = FALSE),
                          actionButton("run_spearman", "محاسبه همبستگی اسپیرمن")
                   ),
                   column(8,
                          plotOutput("spearman_plot"),
                          verbatimTextOutput("spearman_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۷"), "مثال‌های کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("مثال ۱: رابطه سن و فشار خون"),
                              tags$ul(
                                tags$li(tags$b("متغیرها:"), "سن (کمی) و فشار خون سیستولیک (کمی)"),
                                tags$li(tags$b("توزیع:"), "نرمال"),
                                tags$li(tags$b("آزمون:"), "همبستگی پیرسون"),
                                tags$li(tags$b("نتایج:"), "r = 0.65, p < 0.001"),
                                tags$li(tags$b("تفسیر:"), "رابطه مثبت و قوی بین سن و فشار خون وجود دارد")
                              )
                          )
                   ),
                   column(6,
                          div(class = "warning-box",
                              h4("مثال ۲: رابطه سطح درد و رضایت بیمار"),
                              tags$ul(
                                tags$li(tags$b("متغیرها:"), "سطح درد (ترتیبی) و رضایت (ترتیبی)"),
                                tags$li(tags$b("توزیع:"), "غیرنرمال"),
                                tags$li(tags$b("آزمون:"), "همبستگی اسپیرمن"),
                                tags$li(tags$b("نتایج:"), "ρ = -0.72, p < 0.001"),
                                tags$li(tags$b("تفسیر:"), "رابطه منفی و قوی بین درد و رضایت وجود دارد")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۸"), "اشتباهات رایج در تحلیل همبستگی"
                 ),
                 div(class = "warning-box",
                     h4("هشدارهای مهم"),
                     tags$ul(
                       tags$li(tags$b("اشتباه علیت:"), "همبستگی به معنای علیت نیست"),
                       tags$li(tags$b("تأثیر متغیرهای مخدوشگر:"), "متغیرهای سوم می‌توانند رابطه مشاهده شده را توضیح دهند"),
                       tags$li(tags$b("رابطه غیرخطی:"), "همبستگی فقط رابطه خطی را اندازه می‌گیرد"),
                       tags$li(tags$b("تأثیر مقادیر پرت:"), "مقادیر پرت می‌توانند همبستگی را به شدت تحت تأثیر قرار دهند"),
                       tags$li(tags$b("دامنه محدود:"), "اگر دامنه یکی از متغیرها محدود باشد، همبستگی دست کم گرفته می‌شود")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۹"), "راهنمای انتخاب آزمون همبستگی"
                 ),
                 tableOutput("correlation_selection_guide"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۱۰"), "بررسی پیش‌فرض‌های همبستگی پیرسون"
                 ),
                 div(class = "info-box",
                     h4("روش‌های بررسی پیش‌فرض‌ها"),
                     tags$ul(
                       tags$li(tags$b("نرمال بودن:"),
                               tags$ul(
                                 tags$li("آزمون شاپیرو-ویلک برای هر متغیر"),
                                 tags$li("نمودار Q-Q برای هر متغیر"),
                                 tags$li("هیستوگرام و منحنی نرمال")
                               )),
                       tags$li(tags$b("خطی بودن:"),
                               tags$ul(
                                 tags$li("نمودار پراکندگی"),
                                 tags$li("بررسی الگوی نقاط"),
                                 tags$li("عدم وجود الگوی منحنی")
                               )),
                       tags$li(tags$b("همسانی واریانس:"),
                               tags$ul(
                                 tags$li("پراکندگی یکنواخت نقاط حول خط روند"),
                                 tags$li("عدم وجود الگوی قیفی شکل")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۱۱"), "اندازه اثر در همبستگی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h5("ضریب تعیین (R²)"),
                              tags$ul(
                                tags$li("مربع ضریب همبستگی"),
                                tags$li("نشان‌دهنده درصد واریانس تبیین‌شده"),
                                tags$li("تفسیر:"),
                                tags$ul(
                                  tags$li("R² = 0.01: اثر کوچک (۱٪ واریانس)"),
                                  tags$li("R² = 0.09: اثر متوسط (۹٪ واریانس)"),
                                  tags$li("R² = 0.25: اثر بزرگ (۲۵٪ واریانس)")
                                )
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h5("توان آماری"),
                              tags$ul(
                                tags$li("احتمال تشخیص همبستگی واقعی"),
                                tags$li("وابسته به:"),
                                tags$ul(
                                  tags$li("حجم نمونه"),
                                  tags$li("اندازه اثر"),
                                  tags$li("سطح معنی‌داری")
                                ),
                                tags$li("برای همبستگی متوسط (r = 0.3):"),
                                tags$ul(
                                  tags$li("n = 30 → توان ≈ ۳۰٪"),
                                  tags$li("n = 100 → توان ≈ ۸۰٪"),
                                  tags$li("n = 200 → توان ≈ ۹۵٪")
                                )
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۱۲"), "نکات عملی برای پژوهشگران"
                 ),
                 div(class = "highlight-box",
                     h4("توصیه‌های مهم"),
                     tags$ul(
                       tags$li("همیشه نمودار پراکندگی رسم کنید"),
                       tags$li("پیش‌فرض‌های آزمون را بررسی کنید"),
                       tags$li("برای داده‌های غیرنرمال از اسپیرمن استفاده کنید"),
                       tags$li("ضریب تعیین (R²) را گزارش دهید"),
                       tags$li("فاصله اطمینان برای ضریب همبستگی گزارش دهید"),
                       tags$li("در تفسیر نتایج محتاط باشید و از نتیجه‌گیری علی خودداری کنید")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۱.۱۳"), "گزارش نتایج"
                 ),
                 div(class = "info-box",
                     h4("قالب استاندارد گزارش نتایج"),
                     h5("برای همبستگی پیرسون:"),
                     p("r(درجه آزادی) = ضریب همبستگی, p = مقدار p, 95% CI [حد پایین, حد بالا]"),
                     p("مثال: r(98) = 0.65, p < 0.001, 95% CI [0.52, 0.75]"),
                     
                     h5("برای همبستگی اسپیرمن:"),
                     p("ρ(درجه آزادی) = ضریب همبستگی, p = مقدار p"),
                     p("مثال: ρ(98) = 0.72, p < 0.001"),
                     
                     h5("تفسیر:"),
                     p("نتایج نشان داد رابطه مثبت و معنی‌داری بین سن و فشار خون وجود دارد (r = 0.65, p < 0.001). این رابطه قوی نشان می‌دهد که با افزایش سن، فشار خون نیز تمایل به افزایش دارد.")
                 )
             )
           ),
           
           "linear_regression" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۱۲: رگرسیون خطی"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۱"), "مقدمه رگرسیون خطی"
                 ),
                 div(class = "highlight-box",
                     h4("تعریف رگرسیون خطی"),
                     p("رگرسیون خطی روشی آماری برای مدل‌سازی رابطه بین یک متغیر وابسته کمی و یک یا چند متغیر مستقل است. هدف پیش‌بینی متغیر وابسته بر اساس متغیرهای مستقل است."),
                     tags$ul(
                       tags$li(tags$b("متغیر وابسته (Response):"), "متغیری که می‌خواهیم پیش‌بینی کنیم"),
                       tags$li(tags$b("متغیر مستقل (Predictor):"), "متغیری که برای پیش‌بینی استفاده می‌کنیم"),
                       tags$li(tags$b("مثال‌های پزشکی:"),
                               tags$ul(
                                 tags$li("پیش‌بینی فشار خون بر اساس سن و وزن"),
                                 tags$li("پیش‌بینی سطح قند خون بر اساس رژیم غذایی"),
                                 tags$li("پیش‌بینی زمان بهبودی بر اساس شدت بیماری")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۲"), "انواع رگرسیون خطی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("رگرسیون خطی ساده"),
                              tags$ul(
                                tags$li("یک متغیر مستقل و یک متغیر وابسته"),
                                tags$li(tags$b("مدل:"), "y = β₀ + β₁x + ε"),
                                tags$li(tags$b("اجزا:"),
                                        tags$ul(
                                          tags$li("β₀: عرض از مبدأ (Intercept)"),
                                          tags$li("β₁: شیب (Slope)"),
                                          tags$li("ε: خطا (Error)")
                                        )),
                                tags$li(tags$b("مثال:"), "پیش‌بینی فشار خون بر اساس سن")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("رگرسیون خطی چندگانه"),
                              tags$ul(
                                tags$li("چند متغیر مستقل و یک متغیر وابسته"),
                                tags$li(tags$b("مدل:"), "y = β₀ + β₁x₁ + β₂x₂ + ... + βₚxₚ + ε"),
                                tags$li(tags$b("مزایا:"),
                                        tags$ul(
                                          tags$li("کنترل اثر متغیرهای مخدوشگر"),
                                          tags$li("بررسی اثرات مستقل متغیرها"),
                                          tags$li("پیش‌بینی دقیق‌تر")
                                        )),
                                tags$li(tags$b("مثال:"), "پیش‌بینی فشار خون بر اساس سن، وزن و جنسیت")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۳"), "مفاهیم پایه در رگرسیون"
                 ),
                 div(class = "warning-box",
                     h4("شاخص‌های مهم رگرسیون"),
                     tags$ul(
                       tags$li(tags$b("ضریب تعیین (R²):"), "نسبت واریانس تبیین‌شده به واریانس کل"),
                       tags$li(tags$b("ضریب تعیین تعدیل‌شده:"), "R² با تعدیل برای تعداد متغیرها"),
                       tags$li(tags$b("خطای معیار برآورد:"), "میانگین فاصله نقاط از خط رگرسیون"),
                       tags$li(tags$b("آماره F:"), "معنی‌داری کلی مدل"),
                       tags$li(tags$b("ضرایب استانداردشده:"), "برای مقایسه اثر متغیرها")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۴"), "پیش‌فرض‌های رگرسیون خطی"
                 ),
                 div(class = "info-box",
                     h4("پیش‌فرض‌های اصلی"),
                     tags$ul(
                       tags$li(tags$b("خطی بودن:"), "رابطه بین متغیرها خطی است"),
                       tags$li(tags$b("استقلال خطاها:"), "خطاها از هم مستقل هستند"),
                       tags$li(tags$b("همسانی واریانس:"), "واریانس خطاها ثابت است"),
                       tags$li(tags$b("نرمال بودن خطاها:"), "خطاها توزیع نرمال دارند"),
                       tags$li(tags$b("عدم همخطی:"), "متغیرهای مستقل همبستگی بالایی ندارند")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۵"), "شبیه‌ساز رگرسیون خطی ساده"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات مدل"),
                          sliderInput("slr_slope", "شیب (β₁):", 
                                      min = -2, max = 2, value = 0.8, step = 0.1),
                          sliderInput("slr_intercept", "عرض از مبدأ (β₀):", 
                                      min = -10, max = 10, value = 2, step = 1),
                          numericInput("slr_sample_size", "حجم نمونه:", 
                                       value = 100, min = 10, max = 1000),
                          sliderInput("slr_noise", "درجه نویز:", 
                                      min = 0.1, max = 3, value = 1, step = 0.1),
                          actionButton("run_slr", "اجرای رگرسیون ساده")
                   ),
                   column(8,
                          plotOutput("slr_plot"),
                          verbatimTextOutput("slr_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۶"), "شبیه‌ساز رگرسیون خطی چندگانه"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات مدل"),
                          numericInput("mlr_sample_size", "حجم نمونه:", 
                                       value = 100, min = 10, max = 1000),
                          
                          h5("ضرایب متغیرها:"),
                          sliderInput("mlr_beta1", "ضریب X₁:", 
                                      min = -2, max = 2, value = 0.7, step = 0.1),
                          sliderInput("mlr_beta2", "ضریب X₂:", 
                                      min = -2, max = 2, value = 0.5, step = 0.1),
                          sliderInput("mlr_intercept", "عرض از مبدأ:", 
                                      min = -10, max = 10, value = 3, step = 1),
                          
                          sliderInput("mlr_correlation", "همبستگی بین X₁ و X₂:", 
                                      min = -0.8, max = 0.8, value = 0.3, step = 0.1),
                          actionButton("run_mlr", "اجرای رگرسیون چندگانه")
                   ),
                   column(8,
                          plotOutput("mlr_plot"),
                          verbatimTextOutput("mlr_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۷"), "مثال‌های کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("مثال ۱: رگرسیون ساده"),
                              tags$ul(
                                tags$li(tags$b("سوال:"), "آیا سن می‌تواند فشار خون را پیش‌بینی کند؟"),
                                tags$li(tags$b("متغیر وابسته:"), "فشار خون سیستولیک"),
                                tags$li(tags$b("متغیر مستقل:"), "سن"),
                                tags$li(tags$b("نتایج:"), "R² = 0.42, β = 0.65, p < 0.001"),
                                tags$li(tags$b("تفسیر:"), "سن ۴۲٪ از واریانس فشار خون را تبیین می‌کند")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("مثال ۲: رگرسیون چندگانه"),
                              tags$ul(
                                tags$li(tags$b("سوال:"), "چه عواملی سطح قند خون را پیش‌بینی می‌کنند؟"),
                                tags$li(tags$b("متغیر وابسته:"), "سطح قند خون ناشتا"),
                                tags$li(tags$b("متغیرهای مستقل:"), "سن، BMI، سابقه خانوادگی"),
                                tags$li(tags$b("نتایج:"), "R² = 0.58, همه متغیرها معنی‌دار"),
                                tags$li(tags$b("تفسیر:"), "مدل ۵۸٪ از واریانس قند خون را تبیین می‌کند")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۸"), "تشخیص مشکلات در رگرسیون"
                 ),
                 div(class = "warning-box",
                     h4("مشکلات رایج و راه‌حل‌ها"),
                     tags$ul(
                       tags$li(tags$b("همخطی (Multicollinearity):"),
                               tags$ul(
                                 tags$li("علائم: ضرایب ناپایدار، علائم غیرمنطقی"),
                                 tags$li("تشخیص: VIF > 10"),
                                 tags$li("راه‌حل: حذف متغیرهای همبسته، PCA")
                               )),
                       tags$li(tags$b("نقض همسانی واریانس:"),
                               tags$ul(
                                 tags$li("علائم: الگوی قیفی در نمودار residuals"),
                                 tags$li("تشخیص: آزمون بروش-پاگان"),
                                 tags$li("راه‌حل: تبدیل متغیر، رگرسیون وزنی")
                               )),
                       tags$li(tags$b("مقادیر پرت:"),
                               tags$ul(
                                 tags$li("تشخیص: نمودار residuals، فاصله کوک"),
                                 tags$li("راه‌حل: بررسی داده، حذف یا تبدیل")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۲.۹"), "گزارش نتایج رگرسیون"
                 ),
                 div(class = "highlight-box",
                     h4("قالب استاندارد گزارش"),
                     h5("برای رگرسیون ساده:"),
                     p("مدل رگرسیون معنی‌دار بود (F(1, 98) = 45.6, p < 0.001) و ۳۲٪ از واریانس متغیر وابسته را تبیین کرد. سن پیش‌بین معنی‌دار فشار خون بود (β = 0.65, p < 0.001)."),
                     
                     h5("برای رگرسیون چندگانه:"),
                     p("مدل رگرسیون معنی‌دار بود (F(3, 96) = 32.8, p < 0.001, R² = 0.51). سن (β = 0.42, p < 0.001)، BMI (β = 0.28, p = 0.01) و سابقه خانوادگی (β = 0.35, p < 0.001) پیش‌بین‌های معنی‌دار بودند."),
                     
                     h5("جدول ضرایب:"),
                     p("همیشه جدول ضرایب با مقادیر β، خطای استاندارد، آماره t و p-value گزارش شود.")
                 )
             )
           ),
           
           "logistic_regression" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۱۳: رگرسیون لجستیک"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۳.۱"), "مقدمه رگرسیون لجستیک"
                 ),
                 div(class = "highlight-box",
                     h4("تعریف رگرسیون لجستیک"),
                     p("رگرسیون لجستیک برای مدل‌سازی رابطه بین یک متغیر وابسته دوحالتی (باینری) و یک یا چند متغیر مستقل استفاده می‌شود. این روش احتمال رخداد یک رویداد را پیش‌بینی می‌کند."),
                     tags$ul(
                       tags$li(tags$b("متغیر وابسته:"), "دوحالتی (۰ و ۱)"),
                       tags$li(tags$b("خروجی:"), "احتمال بین ۰ و ۱"),
                       tags$li(tags$b("مثال‌های پزشکی:"),
                               tags$ul(
                                 tags$li("پیش‌بینی خطر بیماری قلبی"),
                                 tags$li("عوامل مؤثر بر پاسخ به درمان"),
                                 tags$li("پیش‌بینی مرگ و میر بیماران")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۳.۲"), "انواع رگرسیون لجستیک"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("رگرسیون لجستیک ساده"),
                              tags$ul(
                                tags$li("یک متغیر مستقل و یک متغیر وابسته دوحالتی"),
                                tags$li(tags$b("مدل:"), "logit(p) = ln(p/(1-p)) = β₀ + β₁x"),
                                tags$li(tags$b("تابع پیوند:"), "تابع logit"),
                                tags$li(tags$b("مثال:"), "پیش‌بینی دیابت بر اساس سن")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("رگرسیون لجستیک چندگانه"),
                              tags$ul(
                                tags$li("چند متغیر مستقل و یک متغیر وابسته دوحالتی"),
                                tags$li(tags$b("مدل:"), "logit(p) = β₀ + β₁x₁ + β₂x₂ + ..."),
                                tags$li(tags$b("مزایا:"),
                                        tags$ul(
                                          tags$li("کنترل اثر متغیرهای مخدوشگر"),
                                          tags$li("بررسی اثرات مستقل عوامل خطر"),
                                          tags$li("محاسبه odds ratio تعدیل‌شده")
                                        )),
                                tags$li(tags$b("مثال:"), "پیش‌بینی دیابت بر اساس سن، BMI و سابقه خانوادگی")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۳.۳"), "مفاهیم پایه در رگرسیون لجستیک"
                 ),
                 div(class = "warning-box",
                     h4("شاخص‌های مهم"),
                     tags$ul(
                       tags$li(tags$b("احتمال (Probability):"), "شانس رخداد رویداد (۰ تا ۱)"),
                       tags$li(tags$b("شانس (Odds):"), "نسبت احتمال رخداد به عدم رخداد"),
                       tags$li(tags$b("لاجیت (Logit):"), "لگاریتم طبیعی شانس"),
                       tags$li(tags$b("نسبت شانس (Odds Ratio):"), "تغییر شانس به ازای یک واحد تغییر در متغیر مستقل"),
                       tags$li(tags$b("آماره -2LogLikelihood:"), "معیار برازش مدل")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۳.۴"), "شبیه‌ساز رگرسیون لجستیک ساده"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات مدل"),
                          sliderInput("logit_beta", "ضریب متغیر مستقل:", 
                                      min = -3, max = 3, value = 0.8, step = 0.1),
                          sliderInput("logit_intercept", "عرض از مبدأ:", 
                                      min = -5, max = 5, value = -1, step = 0.1),
                          numericInput("logit_sample_size", "حجم نمونه:", 
                                       value = 200, min = 50, max = 1000),
                          sliderInput("logit_base_prob", "احتمال پایه:", 
                                      min = 0.1, max = 0.9, value = 0.3, step = 0.05),
                          actionButton("run_logit", "اجرای رگرسیون لجستیک")
                   ),
                   column(8,
                          plotOutput("logit_plot"),
                          verbatimTextOutput("logit_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۳.۵"), "تفسیر نسبت شانس (Odds Ratio)"
                 ),
                 div(class = "info-box",
                     h4("راهنمای تفسیر OR"),
                     tableOutput("or_interpretation_table"),
                     h5("نکات مهم:"),
                     tags$ul(
                       tags$li("OR = 1: متغیر مستقل اثری ندارد"),
                       tags$li("OR > 1: متغیر مستقل خطر را افزایش می‌دهد"),
                       tags$li("OR < 1: متغیر مستقل خطر را کاهش می‌دهد"),
                       tags$li("فاصله اطمینان OR شامل ۱ نباشد: اثر معنی‌دار است")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۳.۶"), "مثال‌های کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("مثال ۱: عوامل خطر بیماری قلبی"),
                              tags$ul(
                                tags$li(tags$b("متغیر وابسته:"), "بیماری قلبی (دارد/ندارد)"),
                                tags$li(tags$b("متغیرهای مستقل:"), "سن، فشار خون، کلسترول"),
                                tags$li(tags$b("نتایج:"), "سن: OR = 1.8, فشار خون: OR = 2.1"),
                                tags$li(tags$b("تفسیر:"), "با هر سال افزایش سن، شانس بیماری قلبی ۸۰٪ افزایش می‌یابد")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("مثال ۲: پیش‌بینی پاسخ به درمان"),
                              tags$ul(
                                tags$li(tags$b("متغیر وابسته:"), "پاسخ به درمان (موفق/ناموفق)"),
                                tags$li(tags$b("متغیرهای مستقل:"), "سن، شدت بیماری، نوع درمان"),
                                tags$li(tags$b("نتایج:"), "درمان A vs B: OR = 0.4"),
                                tags$li(tags$b("تفسیر:"), "شانس موفقیت درمان A نسبت به B ۶۰٪ کمتر است")
                              )
                          )
                   )
                 )
             )
           ),
           
           "count_regression" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۱۴: رگرسیون شمارشی"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۴.۱"), "مقدمه رگرسیون شمارشی"
                 ),
                 div(class = "highlight-box",
                     h4("تعریف رگرسیون شمارشی"),
                     p("رگرسیون شمارشی برای مدل‌سازی متغیرهای وابسته که تعداد رویدادها را در یک بازه زمانی یا مکانی نشان می‌دهند استفاده می‌شود. این متغیرها مقادیر صحیح و غیرمنفی دارند."),
                     tags$ul(
                       tags$li(tags$b("متغیر وابسته:"), "شمارشی (۰, ۱, ۲, ۳, ...)"),
                       tags$li(tags$b("توزیع:"), "پواسون یا دوجمله‌ای منفی"),
                       tags$li(tags$b("مثال‌های پزشکی:"),
                               tags$ul(
                                 tags$li("تعداد دفعات بستری در بیمارستان"),
                                 tags$li("تعداد عفونت‌های بیمارستانی"),
                                 tags$li("تعداد داروهای مصرفی بیمار")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۴.۲"), "انواع رگرسیون شمارشی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("رگرسیون پواسون"),
                              tags$ul(
                                tags$li("برای داده‌های شمارشی با واریانس برابر میانگین"),
                                tags$li(tags$b("مدل:"), "ln(λ) = β₀ + β₁x₁ + β₂x₂ + ..."),
                                tags$li(tags$b("پیش‌فرض:"), "میانگین = واریانس"),
                                tags$li(tags$b("تابع پیوند:"), "لگاریتمی"),
                                tags$li(tags$b("مثال:"), "مدل‌سازی تعداد عفونت‌های بیمارستانی")
                              )
                          )
                   ),
                   column(6,
                          div(class = "warning-box",
                              h4("رگرسیون دوجمله‌ای منفی"),
                              tags$ul(
                                tags$li("برای داده‌های شمارشی با پراکندگی بیش از حد"),
                                tags$li(tags$b("مدل:"), "مانند پواسون اما با پارامتر پراکندگی اضافی"),
                                tags$li(tags$b("کاربرد:"), "وقتی واریانس > میانگین"),
                                tags$li(tags$b("مزیت:"), "انعطاف‌پذیری بیشتر در مدل‌سازی پراکندگی"),
                                tags$li(tags$b("مثال:"), "مدل‌سازی تعداد دفعات مراجعه به اورژانس")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۴.۳"), "تشخیص پراکندگی بیش از حد"
                 ),
                 div(class = "info-box",
                     h4("روش‌های تشخیص"),
                     tags$ul(
                       tags$li(tags$b("نسبت واریانس به میانگین:"), "اگر > 1.5 باشد پراکندگی بیش از حد وجود دارد"),
                       tags$li(tags$b("آزمون likelihood ratio:"), "مقایسه مدل پواسون و دوجمله‌ای منفی"),
                       tags$li(tags$b("نمودار فراوانی:"), "مقایسه توزیع مشاهده شده با پواسون"),
                       tags$li(tags$b("آماره پراکندگی:"), "مقدار > 1 نشان‌دهنده پراکندگی بیش از حد است")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۴.۴"), "شبیه‌ساز رگرسیون پواسون"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات مدل"),
                          sliderInput("poisson_lambda", "میانگین (λ):", 
                                      min = 0.5, max = 10, value = 3, step = 0.5),
                          sliderInput("poisson_beta", "ضریب متغیر مستقل:", 
                                      min = -1, max = 1, value = 0.2, step = 0.05),
                          numericInput("poisson_sample_size", "حجم نمونه:", 
                                       value = 200, min = 50, max = 1000),
                          sliderInput("poisson_overdispersion", "درجه پراکندگی بیش از حد:", 
                                      min = 1, max = 3, value = 1, step = 0.1),
                          actionButton("run_poisson", "اجرای رگرسیون پواسون")
                   ),
                   column(8,
                          plotOutput("poisson_plot"),
                          verbatimTextOutput("poisson_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۴.۵"), "مثال‌های کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("مثال ۱: عفونت‌های بیمارستانی"),
                              tags$ul(
                                tags$li(tags$b("متغیر وابسته:"), "تعداد عفونت‌های بیمارستانی در ماه"),
                                tags$li(tags$b("متغیرهای مستقل:"), "طول بستری، سن، نوع جراحی"),
                                tags$li(tags$b("مدل:"), "رگرسیون پواسون"),
                                tags$li(tags$b("تفسیر:"), "با هر روز افزایش طول بستری، تعداد عفونت‌ها ۱۵٪ افزایش می‌یابد")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("مثال ۲: مراجعات اورژانس"),
                              tags$ul(
                                tags$li(tags$b("متغیر وابسته:"), "تعداد مراجعه به اورژانس در سال"),
                                tags$li(tags$b("متغیرهای مستقل:"), "سن، بیماری‌های مزمن، دسترسی به پزشک"),
                                tags$li(tags$b("مدل:"), "رگرسیون دوجمله‌ای منفی"),
                                tags$li(tags$b("تفسیر:"), "بیماران با بیماری مزمن ۲.۳ برابر بیشتر به اورژانس مراجعه می‌کنند")
                              )
                          )
                   )
                 )
             )
           ),
           
           "survival_analysis" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۱۵: تحلیل بقا"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۱"), "مقدمه تحلیل بقا"
                 ),
                 div(class = "highlight-box",
                     h4("تعریف تحلیل بقا"),
                     p("تحلیل بقا مجموعه‌ای از روش‌های آماری برای تحلیل داده‌های 'زمان تا وقوع رخداد' است. این روش‌ها زمانی استفاده می‌شوند که متغیر پاسخ، زمان تا وقوع یک رویداد خاص باشد."),
                     tags$ul(
                       tags$li(tags$b("رویداد (Event):"), "اتفاق مورد مطالعه (مثلاً مرگ، عود بیماری، بهبودی)"),
                       tags$li(tags$b("زمان بقا (Survival Time):"), "فاصله زمانی از شروع مطالعه تا وقوع رویداد"),
                       tags$li(tags$b("سانسورشدگی (Censoring):"), "وقتی زمان دقیق رویداد برای برخی افراد مشخص نیست"),
                       tags$li(tags$b("کاربردهای پزشکی:"),
                               tags$ul(
                                 tags$li("مطالعه طول عمر بیماران پس از تشخیص بیماری"),
                                 tags$li("مقایسه اثربخشی درمان‌های مختلف"),
                                 tags$li("شناسایی عوامل پیش‌آگهی بیماری"),
                                 tags$li("بررسی زمان عود بیماری")
                               ))
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۲"), "مفاهیم پایه در تحلیل بقا"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "success-box",
                              h4("تابع بقا (Survival Function)"),
                              tags$ul(
                                tags$li("S(t) = P(T > t)"),
                                tags$li("احتمال زنده ماندن تا زمان t"),
                                tags$li("مقدار بین 0 و 1"),
                                tags$li("تابعی نزولی"),
                                tags$li(tags$b("مثال:"), "احتمال زنده ماندن بیماران سرطانی تا 5 سال پس از تشخیص")
                              )
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("تابع خطر (Hazard Function)"),
                              tags$ul(
                                tags$li("h(t) = lim(Δt→0) P(t ≤ T < t+Δt | T ≥ t)/Δt"),
                                tags$li("نرخ لحظه‌ای وقوع رویداد"),
                                tags$li("ریسک فوری در زمان t"),
                                tags$li(tags$b("مثال:"), "نرخ مرگ بیماران قلبی در ماه اول پس از جراحی")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۳"), "انواع سانسورشدگی"
                 ),
                 div(class = "warning-box",
                     h4("سانسورشدگی راست (Right Censoring)"),
                     tags$ul(
                       tags$li(tags$b("سانسور نوع I:"), "پایان مطالعه قبل از وقوع رویداد برای برخی افراد"),
                       tags$li(tags$b("سانسور نوع II:"), "پایان مطالعه پس از وقوع تعداد مشخصی رویداد"),
                       tags$li(tags$b("سانسور تصادفی:"), "خروج از مطالعه به دلایل مختلف (مهاجرت، از دست دادن پیگیری)"),
                       tags$li(tags$b("نماد:"), "علامت + برای داده‌های سانسور شده")
                     ),
                     h5("مثال:"),
                     p("بیماری که تا پایان مطالعه زنده مانده: زمان 36+ ماه")
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۴"), "تخمین کاپلان-مایر"
                 ),
                 div(class = "info-box",
                     h4("روش کاپلان-مایر"),
                     tags$ul(
                       tags$li("تخمین غیرپارامتری تابع بقا"),
                       tags$li("مناسب برای داده‌های سانسور شده"),
                       tags$li("ایجاد منحنی بقا"),
                       tags$li("محاسبه احتمال بقا در زمان‌های مختلف"),
                       tags$li(tags$b("فرمول:"), "S(t) = Π(1 - dᵢ/nᵢ) برای تمام i که tᵢ ≤ t")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۵"), "آزمون لگرانک"
                 ),
                 div(class = "highlight-box",
                     h4("مقایسه منحنی‌های بقا"),
                     tags$ul(
                       tags$li("مقایسه منحنی‌های بقای دو یا چند گروه"),
                       tags$li("آزمون ناپارامتری"),
                       tags$li("H₀: همه منحنی‌های بقا یکسان هستند"),
                       tags$li("H₁: حداقل دو منحنی با هم تفاوت دارند"),
                       tags$li(tags$b("مثال:"), "مقایسه بقای بیماران با دو روش درمانی مختلف")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۶"), "رگرسیون کاکس"
                 ),
                 div(class = "success-box",
                     h4("مدل خطرات متناسب کاکس"),
                     tags$ul(
                       tags$li("h(t) = h₀(t) × exp(β₁x₁ + β₂x₂ + ... + βₚxₚ)"),
                       tags$li("h₀(t): تابع خطر پایه"),
                       tags$li("βᵢ: ضرایب رگرسیون"),
                       tags$li("xᵢ: متغیرهای پیش‌بین"),
                       tags$li("نسبت خطر (Hazard Ratio): exp(β)"),
                       tags$li("پیش‌فرض: خطرات متناسب (Proportional Hazards)")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۷"), "شبیه‌ساز منحنی کاپلان-مایر"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات گروه‌ها"),
                          numericInput("km_n_groups", "تعداد گروه‌ها:", 
                                       value = 2, min = 1, max = 4),
                          numericInput("km_sample_size", "حجم نمونه هر گروه:", 
                                       value = 100, min = 10, max = 500),
                          
                          h5("میانگین زمان بقا (ماه):"),
                          numericInput("km_group1_mean", "گروه ۱:", value = 24),
                          numericInput("km_group2_mean", "گروه ۲:", value = 36),
                          conditionalPanel(
                            condition = "input.km_n_groups >= 3",
                            numericInput("km_group3_mean", "گروه ۳:", value = 30)
                          ),
                          conditionalPanel(
                            condition = "input.km_n_groups >= 4",
                            numericInput("km_group4_mean", "گروه ۴:", value = 42)
                          ),
                          
                          sliderInput("km_censoring", "درصد سانسورشدگی:", 
                                      min = 0, max = 50, value = 20, step = 5),
                          actionButton("run_km", "محاسبه منحنی بقا")
                   ),
                   column(8,
                          plotOutput("km_plot"),
                          verbatimTextOutput("km_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۸"), "شبیه‌ساز رگرسیون کاکس"
                 ),
                 fluidRow(
                   column(4,
                          h4("تنظیمات مدل"),
                          numericInput("cox_sample_size", "حجم نمونه:", 
                                       value = 200, min = 50, max = 1000),
                          
                          h5("نسبت خطر (Hazard Ratio):"),
                          sliderInput("cox_hr_age", "سن (به ازای 10 سال):", 
                                      min = 0.5, max = 3, value = 1.2, step = 0.1),
                          sliderInput("cox_hr_treatment", "درمان (جدید vs قدیم):", 
                                      min = 0.1, max = 2, value = 0.6, step = 0.1),
                          sliderInput("cox_hr_stage", "مرحله بیماری (پیشرفته vs اولیه):", 
                                      min = 1, max = 5, value = 2.5, step = 0.1),
                          
                          actionButton("run_cox", "انجام رگرسیون کاکس")
                   ),
                   column(8,
                          plotOutput("cox_plot"),
                          verbatimTextOutput("cox_results")
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۹"), "مثال‌های کاربردی در پزشکی"
                 ),
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              h4("مثال ۱: مطالعه سرطان پستان"),
                              tags$ul(
                                tags$li(tags$b("متغیرها:"), "زمان بقا، وضعیت مرگ، سن، مرحله تومور، نوع درمان"),
                                tags$li(tags$b("تحلیل:"), "منحنی کاپلان-مایر + آزمون لگرانک"),
                                tags$li(tags$b("نتایج:"), "تفاوت معنی‌دار در بقای بیماران با درمان‌های مختلف"),
                                tags$li(tags$b("نتیجه:"), "درمان جدید بقای بهتری دارد")
                              )
                          )
                   ),
                   column(6,
                          div(class = "warning-box",
                              h4("مثال ۲: مطالعه بیماران قلبی"),
                              tags$ul(
                                tags$li(tags$b("متغیرها:"), "زمان تا عود بیماری، سن، جنسیت، فشار خون، کلسترول"),
                                tags$li(tags$b("تحلیل:"), "رگرسیون کاکس"),
                                tags$li(tags$b("نتایج:"), "سن و فشار خون عوامل خطر مستقل هستند"),
                                tags$li(tags$b("نتیجه:"), "کنترل فشار خون خطر عود را کاهش می‌دهد")
                              )
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۱۰"), "پیش‌فرض‌های مهم"
                 ),
                 div(class = "warning-box",
                     h4("پیش‌فرض خطرات متناسب"),
                     tags$ul(
                       tags$li("نسبت خطر در طول زمان ثابت است"),
                       tags$li("بررسی با نمودار لگ-لگ (Log-Log Plot)"),
                       tags$li("آزمون شاپیرو (Schoenfeld Residuals)"),
                       tags$li("اقدام در صورت نقض:"),
                       tags$ul(
                         tags$li("استراتیفیکیشن (Stratification)"),
                         tags$li("اضافه کردن برهمکنش با زمان"),
                         tags$li("استفاده از مدل‌های پارامتریک")
                       )
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۱۱"), "گزارش نتایج"
                 ),
                 div(class = "highlight-box",
                     h4("قالب استاندارد گزارش"),
                     h5("برای منحنی کاپلان-مایر:"),
                     p("میانگین بقا گروه A: X ماه (فاصله اطمینان 95%: Y-Z)، گروه B: ..."),
                     p("آزمون لگرانک: χ²(df, N) = مقدار, p = مقدار"),
                     
                     h5("برای رگرسیون کاکس:"),
                     p("نسبت خطر (HR) = مقدار, فاصله اطمینان 95% = [حد پایین, حد بالا], p = مقدار"),
                     p("مثال: سن (به ازای 10 سال): HR = 1.45, 95% CI [1.20-1.75], p < 0.001"),
                     
                     h5("تفسیر:"),
                     p("نتایج نشان داد که درمان جدید با کاهش 40% در خطر مرگ همراه است (HR = 0.60, 95% CI [0.45-0.80], p = 0.001).")
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۱۲"), "خلاصه روش‌های تحلیل بقا"
                 ),
                 tableOutput("survival_methods_table"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۵.۱۳"), "نکات عملی برای پژوهشگران"
                 ),
                 div(class = "info-box",
                     h4("توصیه‌های مهم"),
                     tags$ul(
                       tags$li("داده‌های سانسور شده را به درستی کدگذاری کنید"),
                       tags$li("پیش‌فرض خطرات متناسب را بررسی کنید"),
                       tags$li("از آزمون لگرانک برای مقایسه گروه‌ها استفاده کنید"),
                       tags$li("نسبت خطر و فاصله اطمینان را گزارش دهید"),
                       tags$li("مدل را برای خطی بودن و تاثیرات غیرخطی بررسی کنید"),
                       tags$li("از نمودارهای مناسب برای نمایش نتایج استفاده کنید")
                     )
                 )
             )
           ),
           
           "tips" = tagList(
             div(class = "rtl-text farsi-font",
                 h2("فصل ۱۶: نکات کاربردی و راهنمای عملی"),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۱"), "نکات طراحی مطالعه"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۱.۱"), "تعیین حجم نمونه"
                 ),
                 div(class = "highlight-box",
                     h4("روش‌های تعیین حجم نمونه"),
                     tags$ul(
                       tags$li(tags$b("بر اساس مطالعات مشابه:"), "استفاده از حجم نمونه مطالعات مشابه"),
                       tags$li(tags$b("محاسبات آماری:"), "استفاده از فرمول‌های حجم نمونه"),
                       tags$li(tags$b("نرم‌افزارهای تخصصی:"), "G*Power, PASS, nQuery"),
                       tags$li(tags$b("قاعده کلی:"), "حداقل 30 نمونه در هر گروه")
                     ),
                     h5("عوامل مؤثر بر حجم نمونه:"),
                     tags$ul(
                       tags$li("اندازه اثر مورد انتظار"),
                       tags$li("سطح معنی‌داری (معمولاً 0.05)"),
                       tags$li("توان آماری (معمولاً 0.8)"),
                       tags$li("نوع آزمون آماری"),
                       tags$li("میزان پراکندگی داده‌ها")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۱.۲"), "روش نمونه‌گیری"
                 ),
                 div(class = "info-box",
                     h4("انتخاب روش نمونه‌گیری مناسب"),
                     tags$ul(
                       tags$li(tags$b("مطالعات مقدماتی:"), "نمونه‌گیری در دسترس"),
                       tags$li(tags$b("مطالعات توصیفی:"), "نمونه‌گیری تصادفی ساده یا سیستماتیک"),
                       tags$li(tags$b("مطالعات تحلیلی:"), "نمونه‌گیری طبقه‌ای"),
                       tags$li(tags$b("مطالعات بزرگ:"), "نمونه‌گیری خوشه‌ای")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۲"), "نکات جمع‌آوری داده"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۲.۱"), "طراحی فرم جمع‌آوری داده"
                 ),
                 div(class = "warning-box",
                     h4("اصول طراحی فرم"),
                     tags$ul(
                       tags$li("تعریف عملیاتی واضح برای هر متغیر"),
                       tags$li("استفاده از مقیاس‌های استاندارد"),
                       tags$li("کدگذاری مناسب برای داده‌های کیفی"),
                       tags$li("در نظر گرفتن واحد اندازه‌گیری"),
                       tags$li("پیش‌آزمون فرم")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۲.۲"), "کنترل کیفیت داده"
                 ),
                 div(class = "success-box",
                     h4("روش‌های کنترل کیفیت"),
                     tags$ul(
                       tags$li("بررسی دامنه مقادیر"),
                       tags$li("شناسایی مقادیر پرت"),
                       tags$li("بررسی consistency داده‌ها"),
                       tags$li("double data entry"),
                       tags$li("بررسی missing data")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۳"), "نکات تحلیل داده"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۳.۱"), "بررسی پیش‌فرض‌ها"
                 ),
                 div(class = "info-box",
                     h4("چک‌لیست پیش‌فرض‌ها"),
                     tags$ul(
                       tags$li("نرمال بودن داده‌ها"),
                       tags$li("همسانی واریانس‌ها"),
                       tags$li("خطی بودن روابط"),
                       tags$li("استقلال مشاهدات"),
                       tags$li("عدم همخطی")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۳.۲"), "انتخاب آزمون آماری"
                 ),
                 div(class = "highlight-box",
                     h4("راهنمای انتخاب آزمون"),
                     tags$ul(
                       tags$li(tags$b("داده‌های کمی نرمال:"), "آزمون‌های پارامتری"),
                       tags$li(tags$b("داده‌های کمی غیرنرمال:"), "آزمون‌های ناپارامتری"),
                       tags$li(tags$b("داده‌های کیفی:"), "آزمون کای-دو یا فیشر"),
                       tags$li(tags$b("داده‌های وابسته:"), "آزمون‌های زوجی"),
                       tags$li(tags$b("چند گروه:"), "ANOVA یا کراسکال-والیس")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۴"), "نکات گزارش نتایج"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۴.۱"), "گزارش آمار توصیفی"
                 ),
                 div(class = "warning-box",
                     h4("اصول گزارش آمار توصیفی"),
                     tags$ul(
                       tags$li("برای داده‌های نرمال: میانگین ± انحراف معیار"),
                       tags$li("برای داده‌های غیرنرمال: میانه (دامنه میان چارکی)"),
                       tags$li("برای داده‌های کیفی: تعداد (درصد)"),
                       tags$li("گزارش حجم نمونه برای هر گروه"),
                       tags$li("گزارش missing data")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۴.۲"), "گزارش آمار استنباطی"
                 ),
                 div(class = "success-box",
                     h4("اصول گزارش آمار استنباطی"),
                     tags$ul(
                       tags$li("گزارش دقیق p-value"),
                       tags$li("گزارش فاصله اطمینان"),
                       tags$li("گزارش اندازه اثر"),
                       tags$li("گزارش آماره آزمون و درجه آزادی"),
                       tags$li("پرهیز از گزارش p-value به صورت ستاره‌ای")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۵"), "اشتباهات رایج"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۵.۱"), "اشتباهات مفهومی"
                 ),
                 div(class = "warning-box",
                     h4("اشتباهات رایج مفهومی"),
                     tags$ul(
                       tags$li("تفسیر همبستگی به عنوان علیت"),
                       tags$li("عدم تفکیک معنی‌داری آماری و اهمیت بالینی"),
                       tags$li("استفاده نادرست از 'پذیرش فرض صفر'"),
                       tags$li("بی‌توجهی به خطای نوع دوم"),
                       tags$li("تکیه صرف بر p-value")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۵.۲"), "اشتباهات فنی"
                 ),
                 div(class = "info-box",
                     h4("اشتباهات فنی رایج"),
                     tags$ul(
                       tags$li("استفاده از آزمون پارامتری برای داده‌های غیرنرمال"),
                       tags$li("بی‌توجهی به پیش‌فرض‌های آزمون"),
                       tags$li("انجام multiple comparisons بدون اصلاح"),
                       tags$li("حذف مقادیر پرت بدون بررسی"),
                       tags$li("استفاده نادرست از میانگین برای داده‌های skewed")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۶"), "نرم‌افزارهای آماری"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۶.۱"), "مقایسه نرم‌افزارها"
                 ),
                 div(class = "highlight-box",
                     h4("نرم‌افزارهای رایج"),
                     tags$ul(
                       tags$li(tags$b("SPSS:"), "مناسب برای شروع، رابط کاربری ساده"),
                       tags$li(tags$b("R:"), "قدرتمند، رایگان، انعطاف‌پذیر"),
                       tags$li(tags$b("SAS:"), "حرفه‌ای، گران، در صنعت داروسازی رایج"),
                       tags$li(tags$b("Stata:"), "کاربرپسند، مناسب برای داده‌های اقتصادی-اجتماعی"),
                       tags$li(tags$b("Python:"), "قدرتمند، رایگان، مناسب برای تحلیل‌های پیشرفته")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۶.۲"), "انتخاب نرم‌افزار"
                 ),
                 div(class = "info-box",
                     h4("معیارهای انتخاب"),
                     tags$ul(
                       tags$li("میزان پیچیدگی تحلیل"),
                       tags$li("هزینه و بودجه"),
                       tags$li("مهارت کاربر"),
                       tags$li("نیازهای خاص پروژه"),
                       tags$li("پشتیبانی و جامعه کاربری")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۷"), "منابع آموزشی"
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۷.۱"), "کتاب‌های مرجع"
                 ),
                 div(class = "success-box",
                     h4("کتاب‌های پیشنهادی"),
                     tags$ul(
                       tags$li("'آمار زیستی' - نوشته محمدتقی آیت‌اللهی"),
                       tags$li("'اصول آمار پزشکی' - نوشته محمدرضا محمدی"),
                       tags$li("'Biostatistics: A Foundation for Analysis in the Health Sciences' - Wayne W. Daniel"),
                       tags$li("'Medical Statistics' - Geoffrey R. Norman"),
                       tags$li("'Practical Statistics for Medical Research' - Douglas G. Altman")
                     )
                 ),
                 
                 div(class = "subsection-title",
                     span(class = "subsection-number", "۱۶.۷.۲"), "منابع آنلاین"
                 ),
                 div(class = "warning-box",
                     h4("منابع آموزشی آنلاین"),
                     tags$ul(
                       tags$li("Coursera: Statistics with R"),
                       tags$li("edX: Introduction to Biostatistics"),
                       tags$li("Khan Academy: Statistics and probability"),
                       tags$li("YouTube: StatQuest with Josh Starmer"),
                       tags$li("وبسایت دانشگاه‌های معتبر")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۸"), "راهنمای انتخاب آزمون آماری"
                 ),
                 
                 fluidRow(
                   column(6,
                          div(class = "info-box",
                              h4("برای داده‌های کمی"),
                              tableOutput("quantitative_tests_guide")
                          )
                   ),
                   column(6,
                          div(class = "info-box",
                              h4("برای داده‌های کیفی"),
                              tableOutput("qualitative_tests_guide")
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۹"), "چک‌لیست پایان مطالعه"
                 ),
                 div(class = "highlight-box",
                     h4("چک‌لیست نهایی قبل از ارسال مقاله"),
                     tags$ul(
                       tags$li("✅ پیش‌فرض‌های آزمون‌ها بررسی شده است"),
                       tags$li("✅ حجم نمونه کافی تأمین شده است"),
                       tags$li("✅ روش نمونه‌گیری به درستی توصیف شده است"),
                       tags$li("✅ متغیرها به درستی تعریف شده‌اند"),
                       tags$li("✅ آمار توصیفی به طور کامل گزارش شده است"),
                       tags$li("✅ آمار استنباطی با جزئیات گزارش شده است"),
                       tags$li("✅ اندازه اثر محاسبه و گزارش شده است"),
                       tags$li("✅ فاصله اطمینان گزارش شده است"),
                       tags$li("✅ محدودیت‌های مطالعه ذکر شده است"),
                       tags$li("✅ نتایج در context بالینی تفسیر شده است")
                     )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۱۰"), "ابزارهای محاسباتی"
                 ),
                 
                 fluidRow(
                   column(4,
                          div(class = "info-box",
                              h4("محاسبه حجم نمونه"),
                              numericInput("power", "توان آماری (0.8-0.9):", value = 0.8, min = 0.5, max = 0.95, step = 0.05),
                              numericInput("alpha", "سطح معنی‌داری:", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                              numericInput("effect_size", "اندازه اثر (کوچک=0.2, متوسط=0.5, بزرگ=0.8):", value = 0.5, min = 0.1, max = 1, step = 0.1),
                              actionButton("calc_sample_size", "محاسبه حجم نمونه"),
                              verbatimTextOutput("sample_size_result")
                          )
                   ),
                   column(4,
                          div(class = "success-box",
                              h4("محاسبه توان آماری"),
                              numericInput("sample_size_power", "حجم نمونه:", value = 30),
                              numericInput("alpha_power", "سطح معنی‌داری:", value = 0.05),
                              numericInput("effect_size_power", "اندازه اثر:", value = 0.5),
                              actionButton("calc_power", "محاسبه توان"),
                              verbatimTextOutput("power_result")
                          )
                   ),
                   column(4,
                          div(class = "warning-box",
                              h4("تبدیل اندازه اثر"),
                              selectInput("effect_type", "نوع اندازه اثر:",
                                          choices = c("d کوهن", "r", "η²", "φ")),
                              numericInput("effect_value", "مقدار اندازه اثر:", value = 0.5),
                              actionButton("convert_effect", "تبدیل"),
                              verbatimTextOutput("effect_conversion_result")
                          )
                   )
                 ),
                 
                 div(class = "section-title",
                     span(class = "section-number", "۱۶.۱۱"), "سخن پایانی"
                 ),
                 div(class = "highlight-box",
                     h4("توصیه‌های نهایی"),
                     tags$ul(
                       tags$li("همیشه با متخصص آمار مشورت کنید"),
                       tags$li("تحلیل آماری را از ابتدای مطالعه برنامه‌ریزی کنید"),
                       tags$li("نتایج را در context بالینی تفسیر کنید"),
                       tags$li("شفافیت در گزارش روش‌ها و نتایج"),
                       tags$li("یادگیری مستمر را ادامه دهید")
                     ),
                     p("یادتان باشد: آمار ابزاری است برای کمک به تصمیم‌گیری بهتر، نه جایگزینی برای قضاوت بالینی."),
                     
                     h4("نکته طلایی:"),
                     div(class = "success-box",
                         p("معنی‌داری آماری ≠ اهمیت بالینی"),
                         p("همیشه به دنبال تفسیر عملی و کاربردی نتایج باشید.")
                     )
                 )
             )
           )
    )
  })
  
  # توابع سرور برای جداول و نمودارها
  output$variables_table <- renderTable({
    data.frame(
      "نوع متغیر" = c("کمی پیوسته", "کمی گسسته", "کیفی اسمی", "کیفی ترتیبی"),
      "تعریف" = c(
        "مقادیر عددی با فاصله‌های معنی‌دار - مثال: قد، وزن",
        "مقادیر عددی بدون فاصله‌های معنی‌دار - مثال: تعداد فرزندان",
        "دسته‌های بدون ترتیب - مثال: گروه خونی، جنسیت",
        "دسته‌های با ترتیب طبیعی - مثال: سطح درد، درجه سرطان"
      ),
      "مثال پزشکی" = c(
        "فشار خون، دمای بدن",
        "تعداد بستری‌ها، تعداد داروها",
        "گروه خونی، نوع بیماری",
        "درجه سرطان، سطح ناراحتی"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  output$sigma_lab_data <- renderTable({
    data.frame(
      "نمونه" = 1:5,
      "مقدار قند خون (mg/dL)" = c(95, 102, 98, 116, 135),
      "وضعیت" = c("قابل قبول", "قابل قبول", "قابل قبول", "هشدار", "اقدام")
    )
  }, striped = TRUE, hover = TRUE)
  
  # نمودارهای آمار توصیفی
  output$mean_plot <- renderPlot({
    set.seed(123)
    data <- rnorm(100, mean = 100, sd = 15)
    mean_val <- mean(data)
    
    ggplot(data.frame(x = data), aes(x = x)) +
      geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", alpha = 0.7) +
      geom_density(color = "darkblue", size = 1) +
      geom_vline(xintercept = mean_val, color = "red", size = 1, linetype = "dashed") +
      labs(title = "توزیع داده‌ها با میانگین", x = "مقدار", y = "چگالی") +
      theme_minimal()
  })
  
  output$median_plot <- renderPlot({
    set.seed(123)
    data <- c(rnorm(90, mean = 100, sd = 15), rep(200, 10))
    
    ggplot(data.frame(x = data), aes(x = x)) +
      geom_boxplot(fill = "lightgreen", alpha = 0.7) +
      labs(title = "نمودار جعبه‌ای - مقاوم به مقادیر پرت", x = "مقدار") +
      theme_minimal()
  })
  
  output$mode_plot <- renderPlot({
    data <- c(rep("A", 40), rep("B", 30), rep("C", 20), rep("D", 10))
    freq_data <- as.data.frame(table(data))
    
    ggplot(freq_data, aes(x = data, y = Freq)) +
      geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
      labs(title = "نمودار فراوانی - نمایش نما", x = "دسته", y = "فراوانی") +
      theme_minimal()
  })
  
  # جداول فراوانی
  output$qualitative_freq_table <- renderTable({
    data.frame(
      "گروه خونی" = c("O", "A", "B", "AB"),
      "فراوانی مطلق" = c(45, 35, 15, 5),
      "فراوانی نسبی" = c(0.45, 0.35, 0.15, 0.05),
      "فراوانی درصدی" = c("45%", "35%", "15%", "5%")
    )
  }, striped = TRUE, hover = TRUE)
  
  output$ordinal_freq_table <- renderTable({
    data.frame(
      "درجه سرطان" = c("I", "II", "III", "IV"),
      "فراوانی" = c(20, 35, 25, 10),
      "فراوانی نسبی" = c("22.2%", "38.9%", "27.8%", "11.1%"),
      "فراوانی تجمعی" = c("22.2%", "61.1%", "88.9%", "100%")
    )
  }, striped = TRUE, hover = TRUE)
  
  output$quantitative_freq_table <- renderTable({
    data.frame(
      "بازه سنی" = c("20-30", "30-40", "40-50", "50-60", "60-70"),
      "فراوانی" = c(15, 25, 30, 20, 10),
      "فراوانی نسبی" = c("15%", "25%", "30%", "20%", "10%"),
      "نقطه میانی" = c(25, 35, 45, 55, 65)
    )
  }, striped = TRUE, hover = TRUE)
  
  # نمودارهای توصیفی
  output$bar_chart_demo <- renderPlot({
    data <- data.frame(
      disease = c("قلبی", "سرطان", "تنفسی", "گوارشی"),
      frequency = c(30, 25, 20, 15)
    )
    
    ggplot(data, aes(x = reorder(disease, -frequency), y = frequency)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
      labs(title = "نمودار میله‌ای - شیوع بیماری‌ها", x = "نوع بیماری", y = "فراوانی") +
      theme_minimal()
  })
  
  output$pie_chart_demo <- renderPlot({
    data <- data.frame(
      group = c("A", "B", "O", "AB"),
      value = c(45, 35, 15, 5)
    )
    
    ggplot(data, aes(x = "", y = value, fill = group)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = "نمودار دایره‌ای - توزیع گروه خونی", fill = "گروه خونی") +
      theme_void()
  })
  
  output$histogram_demo <- renderPlot({
    set.seed(123)
    data <- rnorm(100, mean = 120, sd = 15)
    
    ggplot(data.frame(x = data), aes(x = x)) +
      geom_histogram(bins = 15, fill = "lightgreen", alpha = 0.7, color = "black") +
      labs(title = "هیستوگرام - توزیع فشار خون", x = "فشار خون", y = "فراوانی") +
      theme_minimal()
  })
  
  output$boxplot_demo <- renderPlot({
    set.seed(123)
    group1 <- rnorm(50, mean = 100, sd = 10)
    group2 <- rnorm(50, mean = 110, sd = 12)
    
    data <- data.frame(
      value = c(group1, group2),
      group = rep(c("درمان A", "درمان B"), each = 50)
    )
    
    ggplot(data, aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = "نمودار جعبه‌ای - مقایسه دو گروه", x = "گروه درمانی", y = "نتیجه") +
      theme_minimal()
  })
  
  # داده‌های فشار خون
  output$bp_raw_data_table <- renderTable({
    set.seed(123)
    data.frame(
      "بیمار" = 1:10,
      "فشار خون" = round(rnorm(10, mean = 120, sd = 15), 1)
    )
  }, striped = TRUE, hover = TRUE)
  
  output$bp_freq_table <- renderTable({
    data.frame(
      "بازه فشار خون" = c("90-100", "100-110", "110-120", "120-130", "130-140", "140-150"),
      "فراوانی" = c(5, 12, 28, 35, 15, 5),
      "فراوانی درصدی" = c("5%", "12%", "28%", "35%", "15%", "5%")
    )
  }, striped = TRUE, hover = TRUE)
  
  # خلاصه آمار توصیفی
  output$descriptive_summary_table <- renderTable({
    data.frame(
      "شاخص" = c("میانگین", "میانه", "نما", "انحراف معیار", "دامنه", "دامنه میان چارکی"),
      "تعریف" = c(
        "مجموع مقادیر تقسیم بر تعداد",
        "مقدار وسطی داده‌های مرتب شده",
        "پرتکرارترین مقدار",
        "میانگین فاصله از میانگین",
        "تفاوت بزرگترین و کوچکترین مقدار",
        "تفاوت چارک سوم و اول"
      ),
      "کاربرد" = c(
        "داده‌های نرمال و متقارن",
        "داده‌های غیرنرمال و دارای مقادیر پرت",
        "داده‌های کیفی و ترتیبی",
        "اندازه‌گیری پراکندگی",
        "بررسی اولیه پراکندگی",
        "شناسایی مقادیر پرت"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # شبیه‌ساز توزیع نرمال
  observeEvent(input$plot_normal, {
    output$normal_plot <- renderPlot({
      req(input$norm_mean, input$norm_sd, input$norm_sample_size)
      
      # نمونه‌گیری تصادفی از توزیع نرمال
      set.seed(123)
      data <- rnorm(input$norm_sample_size, mean = input$norm_mean, sd = input$norm_sd)
      
      # محدوده ثابت برای محور x - برای مقایسه بهتر
      fixed_x_min <- 0
      fixed_x_max <- 200
      
      # محدوده برای منحنی نظری
      x_seq <- seq(fixed_x_min, fixed_x_max, length.out = 400)
      y_density <- dnorm(x_seq, mean = input$norm_mean, sd = input$norm_sd)
      
      # محاسبه ارتفاع برای قرار دادن متن‌ها
      max_density <- max(y_density)
      
      # ایجاد نمودار با محدوده ثابت
      p <- ggplot(data.frame(x = data), aes(x = x)) +
        # هیستوگرام داده‌های شبیه‌سازی شده
        geom_histogram(aes(y = ..density..), 
                       bins = 30, 
                       fill = "lightblue", 
                       alpha = 0.7,
                       color = "black") +
        # منحنی چگالی داده‌های شبیه‌سازی شده (آبی)
        geom_density(color = "darkblue", size = 1.5, alpha = 0.7) +
        # منحنی نظری نرمال (قرمز)
        geom_line(data = data.frame(x = x_seq, y = y_density),
                  aes(x = x, y = y), 
                  color = "red", 
                  size = 1.8, 
                  linetype = "solid",
                  alpha = 0.8) +
        # خط میانگین
        geom_vline(xintercept = input$norm_mean, 
                   color = "red", 
                   size = 2,
                   alpha = 0.8) +
        # خطوط انحراف معیار
        geom_vline(xintercept = c(input$norm_mean - input$norm_sd, 
                                  input$norm_mean + input$norm_sd), 
                   color = "darkgreen", 
                   size = 1.5, 
                   linetype = "dashed",
                   alpha = 0.8) +
        # محدوده ثابت برای مقایسه بهتر
        coord_cartesian(xlim = c(fixed_x_min, fixed_x_max)) +
        labs(title = paste("شبیه‌سازی توزیع نرمال -", 
                           input$norm_sample_size, "نمونه"),
             subtitle = paste("μ =", input$norm_mean, ", σ =", input$norm_sd),
             x = "مقدار", 
             y = "چگالی") +
        theme_minimal() +
        theme(
          text = element_text(family = "Tahoma"),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 12, color = "darkred"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
      
      # اضافه کردن راهنما و حاشیه‌نویسی با توضیحات کامل
      p <- p + 
        # منطقه تحت منحنی نظری
        geom_area(data = data.frame(x = x_seq, y = y_density),
                  aes(x = x, y = y), 
                  fill = "red", 
                  alpha = 0.1) +
        
        # توضیحات منحنی قرمز (نظری)
        annotate("label", x = 150, y = max_density * 0.9,
                 label = "📊 منحنی قرمز (نظری):\n• توزیع نرمال ایده‌آل\n• فرمول ریاضی دقیق\n• پایه محاسبات آماری",
                 color = "red", size = 4, hjust = 0, 
                 fill = "pink", alpha = 0.8) +
        
        # توضیحات منحنی آبی (تجربی)
        annotate("label", x = 150, y = max_density * 0.6,
                 label = "📈 منحنی آبی (تجربی):\n• داده‌های شبیه‌سازی شده\n• نمایش واقعی نمونه\n• تحت تأثیر تصادف",
                 color = "darkblue", size = 4, hjust = 0,
                 fill = "lightblue", alpha = 0.8) +
        
        # مقایسه دو منحنی
        annotate("label", x = 150, y = max_density * 0.3,
                 label = "⚖️ مقایسه دو منحنی:\n• انطباق ≈ دقت نمونه‌گیری\n• اختلاف ≈ خطای نمونه‌گیری\n• با n بزرگتر → انطباق بهتر",
                 color = "purple", size = 4, hjust = 0,
                 fill = "lavender", alpha = 0.8) +
        
        # متن راهنما برای خطوط
        annotate("text", x = input$norm_mean, y = max_density * 0.95,
                 label = "μ", color = "red", size = 6, fontface = "bold") +
        annotate("text", x = input$norm_mean + input$norm_sd, y = max_density * 0.8,
                 label = "μ+σ", color = "darkgreen", size = 4, fontface = "bold") +
        annotate("text", x = input$norm_mean - input$norm_sd, y = max_density * 0.8,
                 label = "μ-σ", color = "darkgreen", size = 4, fontface = "bold")
      
      p
    })
    
    output$normal_info <- renderPrint({
      req(input$norm_mean, input$norm_sd, input$norm_sample_size)
      
      set.seed(123)
      data <- rnorm(input$norm_sample_size, mean = input$norm_mean, sd = input$norm_sd)
      
      cat("🎯 توضیحات کامل درباره منحنی‌ها\n")
      cat("=====================================\n\n")
      
      cat("🔴 منحنی قرمز (تئوری):\n")
      cat("   • نشان‌دهنده توزیع نرمال ایده‌آل و کامل است\n")
      cat("   • بر اساس فرمول ریاضی دقیق محاسبه می‌شود:\n")
      cat("     f(x) = (1/σ√(2π)) * e^(-(x-μ)²/(2σ²))\n")
      cat("   • نشان می‌دهد جامعه آماری چگونه باید باشد\n")
      cat("   • برای محاسبات دقیق و پیش‌بینی استفاده می‌شود\n\n")
      
      cat("🔵 منحنی آبی (تجربی):\n")
      cat("   • از داده‌های شبیه‌سازی شده ایجاد می‌شود\n")
      cat("   • نمایش‌دهنده واقعیت نمونه‌گیری است\n")
      cat("   • تحت تأثیر تصادف و خطای نمونه‌گیری است\n")
      cat("   • هر بار اجرا ممکن است کمی متفاوت باشد\n\n")
      
      cat("⚖️ مقایسه دو منحنی:\n")
      cat("   • اگر دو منحنی شبیه باشند: نمونه نماینده است\n")
      cat("   • اختلاف زیاد: ممکن است نمونه بایاس داشته باشد\n")
      cat("   • با افزایش حجم نمونه، منحنی آبی به قرمز نزدیک‌تر می‌شود\n")
      cat("   • این اصل «قضیه حد مرکزی» را نشان می‌دهد\n\n")
      
      cat("📊 آماره‌های نمونه:\n")
      cat("   حجم نمونه:", input$norm_sample_size, "\n")
      cat("   میانگین نمونه:", round(mean(data), 2), "\n")
      cat("   انحراف معیار نمونه:", round(sd(data), 2), "\n")
      cat("   تفاوت با مقادیر نظری:\n")
      cat("     - تفاوت میانگین:", round(abs(mean(data) - input$norm_mean), 2), "\n")
      cat("     - تفاوت انحراف معیار:", round(abs(sd(data) - input$norm_sd), 2), "\n\n")
      
      cat("💡 نکته آموزشی:\n")
      if (input$norm_sample_size < 30) {
        cat("   نمونه کوچک است - اختلاف منحنی‌ها طبیعی است\n")
      } else if (input$norm_sample_size < 100) {
        cat("   نمونه متوسط است - انطباق نسبتاً خوب\n")
      } else {
        cat("   نمونه بزرگ است - انطباق بسیار خوب\n")
      }
    })
  })
  
  # یک پنل آموزشی جداگانه برای توضیحات بیشتر
  output$curve_explanation <- renderUI({
    div(
      class = "info-box",
      h4("🎓 آموزش: تفاوت منحنی قرمز و آبی"),
      
      fluidRow(
        column(6,
               div(
                 class = "warning-box",
                 h5("🔴 منحنی قرمز - تئوری"),
                 tags$ul(
                   tags$li("ایده‌آل و کامل"),
                   tags$li("فرمول ریاضی دقیق"),
                   tags$li("بدون خطا"),
                   tags$li("پایه محاسبات"),
                   tags$li("نمایش جامعه")
                 )
               )
        ),
        column(6,
               div(
                 class = "success-box",
                 h5("🔵 منحنی آبی - تجربی"),
                 tags$ul(
                   tags$li("واقعی و عملی"),
                   tags$li("از داده‌های نمونه"),
                   tags$li("تحت تأثیر تصادف"),
                   tags$li("نمایش نمونه"),
                   tags$li("ممکن است نویز داشته باشد")
                 )
               )
        )
      ),
      
      div(
        class = "highlight-box",
        h5("⚖️ چرا هر دو منحنی مهم هستند؟"),
        p("در آمار، ما همیشه بین تئوری (آنچه باید باشد) و عمل (آنچه هست) در حرکت هستیم:"),
        tags$ul(
          tags$li("منحنی قرمز به ما می‌گوید چه انتظاری باید داشته باشیم"),
          tags$li("منحنی آبی به ما نشان می‌دهد در واقعیت چه اتفاقی افتاده است"),
          tags$li("مقایسه این دو به ما در تشخیص کیفیت نمونه‌گیری کمک می‌کند")
        )
      ),
      
      div(
        class = "info-box",
        h5("📈 نکات عملی برای دانشجویان:"),
        tags$ul(
          tags$li("با حجم نمونه کوچک، اختلاف طبیعی است"),
          tags$li("با افزایش n، منحنی آبی به قرمز نزدیک می‌شود"),
          tags$li("این اصل مهمی در آمار به نام «قضیه حد مرکزی» است"),
          tags$li("در پژوهش‌های واقعی، ما منحنی آبی را داریم و سعی می‌کنیم به قرمز برسیم")
        )
      )
    )
  })
  
  # نمودار مقایسه‌ای برای نشان دادن اثر حجم نمونه
  output$sample_size_effect_plot <- renderPlot({
    # ایجاد چند توزیع با حجم نمونه‌های مختلف
    x_seq <- seq(0, 200, length.out = 400)
    mean_val <- 100
    sd_val <- 15
    
    sample_sizes <- c(10, 30, 100, 1000)
    colors <- c("orange", "blue", "green", "purple")
    labels <- c("n = 10", "n = 30", "n = 100", "n = 1000")
    
    comparison_data <- data.frame()
    
    # منحنی نظری
    theoretical <- data.frame(
      x = x_seq,
      y = dnorm(x_seq, mean = mean_val, sd = sd_val),
      group = "تئوری (n = ∞)",
      color = "red"
    )
    
    for (i in 1:length(sample_sizes)) {
      set.seed(123)
      data <- rnorm(sample_sizes[i], mean = mean_val, sd = sd_val)
      density_est <- density(data, from = 0, to = 200)
      
      temp_df <- data.frame(
        x = density_est$x,
        y = density_est$y,
        group = factor(rep(labels[i], length(density_est$x)), levels = c(labels, "تئوری (n = ∞)")),
        color = rep(colors[i], length(density_est$x))
      )
      comparison_data <- rbind(comparison_data, temp_df)
    }
    
    # ترکیب داده‌ها
    all_data <- rbind(comparison_data, theoretical)
    
    ggplot(all_data, aes(x = x, y = y, color = group, linetype = group)) +
      geom_line(size = 1.2) +
      labs(title = "اثر حجم نمونه بر دقت برآورد",
           subtitle = "با افزایش n، منحنی تجربی به منحنی تئوری نزدیک می‌شود",
           x = "مقدار", 
           y = "چگالی",
           color = "حجم نمونه",
           linetype = "حجم نمونه") +
      scale_color_manual(values = c(colors, "red")) +
      scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "dashed")) +
      coord_cartesian(xlim = c(0, 200)) +
      theme_minimal() +
      theme(
        text = element_text(family = "Tahoma"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom"
      ) +
      annotate("text", x = 150, y = 0.025, 
               label = "قضیه حد مرکزی در عمل!", 
               color = "purple", size = 5, fontface = "bold")
  })
  
  # شبیه‌ساز توزیع دوجمله‌ای
  observeEvent(input$plot_binomial, {
    output$binomial_plot <- renderPlot({
      req(input$binom_n, input$binom_p, input$binom_sample_size)
      
      set.seed(123)
      data <- rbinom(input$binom_sample_size, size = input$binom_n, prob = input$binom_p)
      
      freq_data <- as.data.frame(table(factor(data, levels = 0:input$binom_n)))
      colnames(freq_data) <- c("x", "Freq")
      freq_data$x <- as.numeric(as.character(freq_data$x))
      
      ggplot(freq_data, aes(x = x, y = Freq)) +
        geom_bar(stat = "identity", fill = "lightgreen", alpha = 0.7, color = "black") +
        geom_point(color = "darkgreen", size = 2) +
        labs(title = "توزیع دوجمله‌ای", x = "تعداد موفقیت‌ها", y = "فراوانی") +
        theme_minimal()
    })
    
    output$binomial_info <- renderPrint({
      req(input$binom_n, input$binom_p, input$binom_sample_size)
      
      set.seed(123)
      data <- rbinom(input$binom_sample_size, size = input$binom_n, prob = input$binom_p)
      
      cat("نتایج شبیه‌سازی توزیع دوجمله‌ای:\n")
      cat("میانگین نمونه:", round(mean(data), 2), "\n")
      cat("واریانس نمونه:", round(var(data), 2), "\n")
    })
  })
  
  # شبیه‌ساز توزیع پواسون
  observeEvent(input$plot_poisson, {
    output$poisson_plot <- renderPlot({
      req(input$pois_lambda, input$pois_sample_size)
      
      set.seed(123)
      data <- rpois(input$pois_sample_size, lambda = input$pois_lambda)
      
      max_x <- max(data) + 2
      freq_data <- as.data.frame(table(factor(data, levels = 0:max_x)))
      colnames(freq_data) <- c("x", "Freq")
      freq_data$x <- as.numeric(as.character(freq_data$x))
      
      ggplot(freq_data, aes(x = x, y = Freq)) +
        geom_bar(stat = "identity", fill = "coral", alpha = 0.7, color = "black") +
        geom_point(color = "darkred", size = 2) +
        labs(title = "توزیع پواسون", 
             subtitle = paste("λ =", input$pois_lambda, "- تعداد نمونه:", input$pois_sample_size),
             x = "تعداد رویدادها", y = "فراوانی") +
        theme_minimal() +
        theme(text = element_text(family = "Tahoma"))
    })
    
    output$poisson_info <- renderPrint({
      req(input$pois_lambda, input$pois_sample_size)
      
      set.seed(123)
      data <- rpois(input$pois_sample_size, lambda = input$pois_lambda)
      
      cat("📊 نتایج شبیه‌سازی توزیع پواسون:\n")
      cat("=====================================\n")
      cat("پارامتر λ:", input$pois_lambda, "\n")
      cat("حجم نمونه:", input$pois_sample_size, "\n")
      cat("میانگین نمونه:", round(mean(data), 2), "\n")
      cat("واریانس نمونه:", round(var(data), 2), "\n")
      cat("نسبت واریانس به میانگین:", round(var(data)/mean(data), 2), "\n\n")
      
      cat("🎯 تحلیل نتایج:\n")
      ratio <- var(data)/mean(data)
      if (abs(ratio - 1) < 0.2) {
        cat("✅ واریانس ≈ میانگین - مشخصه توزیع پواسون\n")
      } else if (ratio > 1) {
        cat("⚠️ واریانس > میانگین - پراکندگی بیش از حد\n")
      } else {
        cat("⚠️ واریانس < میانگین - پراکندگی کمتر از حد\n")
      }
    })
  })
  
  # خلاصه توزیع‌ها
  output$distributions_summary_table <- renderTable({
    data.frame(
      "توزیع" = c("نرمال", "دوجمله‌ای", "پواسون"),
      "پارامترها" = c("μ, σ", "n, p", "λ"),
      "میانگین" = c("μ", "np", "λ"),
      "واریانس" = c("σ²", "np(1-p)", "λ"),
      "کاربرد پزشکی" = c(
        "قد، وزن، فشار خون",
        "تعداد موفقیت‌ها در n آزمایش",
        "رویدادهای نادر در واحد زمان"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # جدول انواع خطا
  output$error_types_table <- renderTable({
    data.frame(
      "نوع خطا" = c("خطای نوع اول (α)", "خطای نوع دوم (β)"),
      "تعریف" = c(
        "رد فرض صفر در حالی که درست است",
        "پذیرش فرض صفر در حالی که نادرست است"
      ),
      "احتمال" = c("α (معمولاً 0.05)", "β (معمولاً 0.2)"),
      "مثال پزشکی" = c(
        "تشخیص بیماری در فرد سالم",
        "عدم تشخیص بیماری در فرد بیمار"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # شبیه‌ساز فاصله اطمینان
  observeEvent(input$calc_ci, {
    output$ci_plot <- renderPlot({
      n <- input$ci_n
      mean_val <- input$ci_mean
      sd_val <- input$ci_sd
      conf_level <- as.numeric(input$ci_level)
      
      error <- qt((1 + conf_level)/2, df = n-1) * sd_val / sqrt(n)
      lower <- mean_val - error
      upper <- mean_val + error
      
      set.seed(123)
      sample_means <- replicate(100, mean(rnorm(n, mean = mean_val, sd = sd_val)))
      
      df <- data.frame(
        sample = 1:100,
        mean = sample_means,
        capture = sample_means >= lower & sample_means <= upper
      )
      
      ggplot(df, aes(x = sample, y = mean, color = capture)) +
        geom_point(size = 2) +
        geom_hline(yintercept = mean_val, color = "blue", linetype = "dashed", size = 1) +
        geom_hline(yintercept = lower, color = "red", linetype = "dashed", size = 1) +
        geom_hline(yintercept = upper, color = "red", linetype = "dashed", size = 1) +
        labs(title = paste("فاصله اطمینان", conf_level*100, "%"), x = "شماره نمونه", y = "میانگین نمونه") +
        scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "red")) +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    output$ci_results <- renderPrint({
      n <- input$ci_n
      mean_val <- input$ci_mean
      sd_val <- input$ci_sd
      conf_level <- as.numeric(input$ci_level)
      
      error <- qt((1 + conf_level)/2, df = n-1) * sd_val / sqrt(n)
      lower <- mean_val - error
      upper <- mean_val + error
      
      cat("نتایج فاصله اطمینان:\n")
      cat("سطح اطمینان:", conf_level*100, "%\n")
      cat("فاصله اطمینان: [", round(lower, 2), ", ", round(upper, 2), "]\n")
    })
  })
  
  # شبیه‌ساز آزمون فرض
  observeEvent(input$calc_ht, {
    output$ht_plot <- renderPlot({
      sample_mean <- input$ht_sample_mean
      pop_mean <- input$ht_pop_mean
      sd_val <- input$ht_sd
      n <- input$ht_n
      alpha <- as.numeric(input$ht_alpha)
      
      t_stat <- (sample_mean - pop_mean) / (sd_val / sqrt(n))
      
      x <- seq(-4, 4, length.out = 100)
      y <- dt(x, df = n-1)
      df <- data.frame(x = x, y = y)
      
      critical_value <- qt(1 - alpha/2, df = n-1)
      
      ggplot(df, aes(x = x, y = y)) +
        geom_line(color = "blue", size = 1) +
        geom_area(data = subset(df, x <= -critical_value), aes(x = x, y = y), fill = "red", alpha = 0.5) +
        geom_area(data = subset(df, x >= critical_value), aes(x = x, y = y), fill = "red", alpha = 0.5) +
        geom_vline(xintercept = t_stat, color = "green", size = 1, linetype = "dashed") +
        geom_vline(xintercept = c(-critical_value, critical_value), color = "red", size = 1, linetype = "dashed") +
        labs(title = "توزیع t تحت فرض صفر", x = "آماره t", y = "چگالی") +
        theme_minimal()
    })
    
    output$ht_results <- renderPrint({
      sample_mean <- input$ht_sample_mean
      pop_mean <- input$ht_pop_mean
      sd_val <- input$ht_sd
      n <- input$ht_n
      alpha <- as.numeric(input$ht_alpha)
      
      t_stat <- (sample_mean - pop_mean) / (sd_val / sqrt(n))
      p_value <- 2 * (1 - pt(abs(t_stat), df = n-1))
      
      cat("نتایج آزمون فرض:\n")
      cat("آماره t:", round(t_stat, 4), "\n")
      cat("p-value:", round(p_value, 4), "\n")
      
      if (p_value < alpha) {
        cat("نتیجه: رد فرض صفر - تفاوت معنی‌دار است\n")
      } else {
        cat("نتیجه: عدم رد فرض صفر - تفاوت معنی‌دار نیست\n")
      }
    })
  })
  
  # جدول مفاهیم کلیدی
  output$key_concepts_table <- renderTable({
    data.frame(
      "مفهوم" = c("p-value", "فاصله اطمینان", "سطح معنی‌داری", "توان آزمون"),
      "تعریف" = c(
        "احتمال مشاهده نتایج نمونه یا افراطی‌تر در صورت صحیح بودن فرض صفر",
        "محدوده‌ای که با اطمینان مشخص شامل پارامتر جامعه می‌شود",
        "حداکثر احتمال خطای نوع اول",
        "احتمال رد فرض صفر نادرست"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # راهنمای جامع انتخاب آزمون
  output$comprehensive_test_selection_guide <- renderTable({
    data.frame(
      "نوع داده" = c("کمی نرمال - یک گروه", "کمی نرمال - دو گروه مستقل", 
                     "کمی نرمال - دو گروه وابسته", "کمی غیرنرمال - یک گروه",
                     "کمی غیرنرمال - دو گروه مستقل", "کمی غیرنرمال - دو گروه وابسته",
                     "ترتیبی - دو گروه مستقل", "ترتیبی - دو گروه وابسته"),
      "آزمون پارامتری" = c("t تک نمونه‌ای", "t مستقل", "t زوجی", 
                           "ندارد", "ندارد", "ندارد", "ندارد", "ندارد"),
      "آزمون ناپارامتری" = c("ویلکاکسون", "من-ویتنی", "ویلکاکسون زوجی",
                             "علامت", "من-ویتنی", "ویلکاکسون زوجی",
                             "من-ویتنی", "ویلکاکسون"),
      "پیش‌فرض‌های اصلی" = c(
        "نرمال بودن داده‌ها",
        "نرمال بودن، همسانی واریانس",
        "نرمال بودن تفاضل‌ها",
        "ندارد",
        "ندارد", 
        "ندارد",
        "ندارد",
        "توزیع متقارن تفاضل‌ها"
      ),
      "شرایط استفاده" = c(
        "مقایسه با مقدار ثابت",
        "دو گروه مستقل",
        "دو اندازه‌گیری وابسته",
        "مقایسه با مقدار ثابت",
        "دو گروه مستقل",
        "دو اندازه‌گیری وابسته",
        "دو گروه مستقل با داده ترتیبی",
        "دو اندازه‌گیری وابسته ترتیبی"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # جدول تصمیم‌گیری نرمال بودن
  output$normality_decision_table <- renderTable({
    data.frame(
      "نتیجه آزمون شاپیرو-ویلک" = c("p-value > 0.05", "p-value < 0.05"),
      "تفسیر" = c("داده‌ها نرمال هستند", "داده‌ها غیرنرمال هستند"),
      "اقدام توصیه شده" = c("استفاده از آزمون پارامتری", "استفاده از آزمون ناپارامتری"),
      "ملاحظات" = c(
        "با حجم نمونه بزرگ (>100) می‌توان از آزمون پارامتری استفاده کرد حتی اگر آزمون معنی‌دار باشد",
        "برای داده‌های بسیار غیرنرمال، حتی با حجم نمونه بزرگ از آزمون ناپارامتری استفاده شود"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # خلاصه آزمون‌های پارامتری و ناپارامتری
  output$parametric_nonparametric_summary_table <- renderTable({
    data.frame(
      "ویژگی" = c("نیاز به توزیع نرمال", "نیاز به همسانی واریانس", "قدرت آماری", 
                  "مقاومت به مقادیر پرت", "نوع داده", "مقیاس اندازه‌گیری",
                  "حجم نمونه مورد نیاز", "سادگی تفسیر"),
      "پارامتری" = c("بله", "بله", "بالا", "کم", "کمی", "فاصله‌ای/نسبی",
                     "حداقل 30", "ساده"),
      "ناپارامتری" = c("خیر", "خیر", "متوسط", "بالا", "کمی/کیفی", "اسمی/ترتیبی",
                       "حداقل 20", "متوسط")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # شبیه‌ساز بررسی نرمال بودن (بهبود یافته)
  observeEvent(input$run_norm_check, {
    output$norm_check_plot <- renderPlot({
      set.seed(123)
      n <- input$norm_check_n
      
      # تولید داده‌ها با چولگی و کشیدگی مشخص
      if (input$norm_check_skew == 0 & input$norm_check_kurtosis == 0) {
        data <- rnorm(n, input$norm_check_mean, input$norm_check_sd)
      } else {
        # استفاده از توزیع skew-normal برای تولید داده‌های با چولگی
        data <- rsn(n, xi = input$norm_check_mean, omega = input$norm_check_sd, 
                    alpha = input$norm_check_skew * 10)
        
        # تنظیم کشیدگی (ساده‌سازی شده)
        if (input$norm_check_kurtosis > 0) {
          data <- data * (1 + input$norm_check_kurtosis/10)
        }
      }
      
      # ایجاد نمودارهای ترکیبی
      p1 <- ggplot(data.frame(x = data), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", alpha = 0.7) +
        geom_density(color = "blue", size = 1) +
        stat_function(fun = dnorm, args = list(mean = mean(data), sd = sd(data)), 
                      color = "red", size = 1, linetype = "dashed") +
        labs(title = "هیستوگرام و منحنی چگالی", x = "مقدار", y = "چگالی") +
        theme_minimal()
      
      p2 <- ggplot(data.frame(x = data), aes(sample = x)) +
        stat_qq() +
        stat_qq_line() +
        labs(title = "نمودار Q-Q", x = "مقادیر تئوری", y = "مقادیر مشاهده شده") +
        theme_minimal()
      
      grid.arrange(p1, p2, ncol = 2)
    })
    
    output$norm_check_results <- renderPrint({
      set.seed(123)
      n <- input$norm_check_n
      
      if (input$norm_check_skew == 0 & input$norm_check_kurtosis == 0) {
        data <- rnorm(n, input$norm_check_mean, input$norm_check_sd)
      } else {
        data <- rsn(n, xi = input$norm_check_mean, omega = input$norm_check_sd, 
                    alpha = input$norm_check_skew * 10)
        if (input$norm_check_kurtosis > 0) {
          data <- data * (1 + input$norm_check_kurtosis/10)
        }
      }
      
      # محاسبه آماره‌ها
      shapiro_test <- shapiro.test(data)
      skewness_val <- moments::skewness(data)
      kurtosis_val <- moments::kurtosis(data)
      
      cat("نتایج بررسی نرمال بودن:\n\n")
      cat("📊 آماره‌های توصیفی:\n")
      cat("میانگین:", round(mean(data), 2), "\n")
      cat("انحراف معیار:", round(sd(data), 2), "\n")
      cat("چولگی:", round(skewness_val, 3), "\n")
      cat("کشیدگی:", round(kurtosis_val, 3), "\n\n")
      
      cat("🔍 آزمون شاپیرو-ویلک:\n")
      cat("آماره W:", round(shapiro_test$statistic, 4), "\n")
      cat("p-value:", format.pval(shapiro_test$p.value, digits = 3), "\n\n")
      
      cat("🎯 تصمیم‌گیری:\n")
      if (shapiro_test$p.value > 0.05) {
        cat("✅ داده‌ها نرمال هستند (p > 0.05)\n")
        cat("پیشنهاد: استفاده از آزمون‌های پارامتری\n")
      } else {
        cat("❌ داده‌ها غیرنرمال هستند (p < 0.05)\n")
        cat("پیشنهاد: استفاده از آزمون‌های ناپارامتری\n")
      }
      
      cat("\n💡 راهنمای تفسیر چولگی و کشیدگی:\n")
      cat("چولگی ایده‌آل: بین -0.5 تا 0.5 (مقدار فعلی:", round(skewness_val, 3), ")\n")
      cat("کشیدگی ایده‌آل: بین 2.5 تا 3.5 (مقدار فعلی:", round(kurtosis_val, 3), ")\n")
    })
  })
  
  
  
  # ادامه توابع برای بخش‌های دیگر...
  
  # راهنمای انتخاب آزمون برای چند گروه
  output$multi_group_test_guide <- renderTable({
    data.frame(
      "نوع داده" = c("کمی نرمال", "کمی غیرنرمال", "کیفی ترتیبی"),
      "تعداد گروه‌ها" = c("≥3", "≥3", "≥3"),
      "آزمون اصلی" = c("ANOVA", "کراسکال-والیس", "کراسکال-والیس"),
      "آزمون تعقیبی" = c("توكی", "دان", "دان"),
      "پیش‌فرض‌ها" = c(
        "نرمال بودن، همسانی واریانس",
        "عدم نیاز به نرمال بودن",
        "عدم نیاز به نرمال بودن"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # شبیه‌ساز ANOVA
  observeEvent(input$run_anova, {
    output$anova_plot <- renderPlot({
      # تولید داده‌های شبیه‌سازی شده
      set.seed(123)
      n_groups <- input$anova_n_groups
      n_per_group <- input$anova_n_per_group
      
      # ایجاد بردارهای میانگین‌ها با استفاده از لیست
      mean_values <- list()
      mean_values[["1"]] <- input$group1_mean
      mean_values[["2"]] <- input$group2_mean
      mean_values[["3"]] <- input$group3_mean
      if (n_groups >= 4) mean_values[["4"]] <- input$group4_mean
      if (n_groups >= 5) mean_values[["5"]] <- input$group5_mean
      if (n_groups >= 6) mean_values[["6"]] <- input$group6_mean
      
      # تولید داده‌ها
      data_list <- list()
      for (i in 1:n_groups) {
        data_list[[i]] <- rnorm(n_per_group, mean = mean_values[[as.character(i)]], sd = input$anova_sd)
      }
      
      data <- data.frame(
        value = unlist(data_list),
        group = factor(rep(paste("گروه", 1:n_groups), each = n_per_group))
      )
      
      ggplot(data, aes(x = group, y = value, fill = group)) +
        geom_boxplot(alpha = 0.7) +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
        labs(title = "مقایسه چند گروه مستقل", 
             subtitle = paste("ANOVA -", n_groups, "گروه"),
             x = "گروه", y = "مقدار") +
        theme_minimal() +
        theme(text = element_text(family = "Tahoma"),
              legend.position = "none")
    })
    
    output$anova_results <- renderPrint({
      # تولید داده‌های شبیه‌سازی شده
      set.seed(123)
      n_groups <- input$anova_n_groups
      n_per_group <- input$anova_n_per_group
      
      # ایجاد بردارهای میانگین‌ها با استفاده از لیست
      mean_values <- list()
      mean_values[["1"]] <- input$group1_mean
      mean_values[["2"]] <- input$group2_mean
      mean_values[["3"]] <- input$group3_mean
      if (n_groups >= 4) mean_values[["4"]] <- input$group4_mean
      if (n_groups >= 5) mean_values[["5"]] <- input$group5_mean
      if (n_groups >= 6) mean_values[["6"]] <- input$group6_mean
      
      # تولید داده‌ها
      data_list <- list()
      for (i in 1:n_groups) {
        data_list[[i]] <- rnorm(n_per_group, mean = mean_values[[as.character(i)]], sd = input$anova_sd)
      }
      
      data <- data.frame(
        value = unlist(data_list),
        group = factor(rep(1:n_groups, each = n_per_group))
      )
      
      # انجام ANOVA
      anova_result <- aov(value ~ group, data = data)
      summary_result <- summary(anova_result)
      
      cat("📊 نتایج تحلیل واریانس (ANOVA):\n")
      cat("=====================================\n")
      cat("تعداد گروه‌ها:", n_groups, "\n")
      cat("تعداد نمونه در هر گروه:", n_per_group, "\n")
      cat("کل نمونه‌ها:", n_groups * n_per_group, "\n\n")
      
      print(summary_result)
      
      # بررسی معنی‌داری
      p_value <- summary_result[[1]]$`Pr(>F)`[1]
      alpha <- as.numeric(input$anova_alpha)
      
      cat("\n🎯 تصمیم‌گیری:\n")
      if (p_value < alpha) {
        cat("✅ رد فرض صفر - حداقل دو گروه با هم تفاوت معنی‌دار دارند\n")
        cat("📌 پیشنهاد: از آزمون تعقیبی (مانند توکی) استفاده کنید\n")
      } else {
        cat("❌ عدم رد فرض صفر - تفاوت معنی‌داری بین گروه‌ها وجود ندارد\n")
      }
      
      # محاسبه اندازه اثر
      ss_between <- summary_result[[1]]$`Sum Sq`[1]
      ss_total <- sum(summary_result[[1]]$`Sum Sq`)
      eta_squared <- ss_between / ss_total
      
      cat("\n📈 اندازه اثر:\n")
      cat("η² (اتا مربع):", round(eta_squared, 3), "\n")
      
      if (eta_squared < 0.01) cat("اثر بسیار کوچک\n")
      else if (eta_squared < 0.06) cat("اثر کوچک\n")
      else if (eta_squared < 0.14) cat("اثر متوسط\n")
      else cat("اثر بزرگ\n")
    })
  })
  
  # راهنمای انتخاب آزمون برای متغیرهای کیفی
  output$categorical_test_selection_guide <- renderTable({
    data.frame(
      "نوع داده" = c("دو متغیر اسمی", "دو متغیر اسمی - نمونه کوچک", 
                     "داده‌های وابسته دوحالتی", "مقایسه نسبت‌ها"),
      "جدول" = c("2×2 یا بزرگتر", "2×2", "2×2", "مقادیر فراوانی"),
      "آزمون" = c("کای-دو", "فیشر", "مک نمار", "آزمون نسبت"),
      "شرایط" = c(
        "فراوانی مورد انتظار ≥5 در 80% خانه‌ها",
        "فراوانی مورد انتظار <5 یا نمونه کوچک",
        "داده‌های قبل-بعد یا جفت‌شده",
        "مقایسه یک یا چند نسبت"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # شبیه‌ساز آزمون کای-دو
  observeEvent(input$run_chi2, {
    output$chi2_plot <- renderPlot({
      # ایجاد ماتریس داده‌ها
      data_matrix <- matrix(c(
        input$chi2_row1_col1, input$chi2_row1_col2,
        input$chi2_row2_col1, input$chi2_row2_col2
      ), nrow = 2, byrow = TRUE)
      
      # تبدیل به داده‌های long format برای نمودار
      data_long <- as.data.frame(as.table(data_matrix))
      colnames(data_long) <- c("Row", "Column", "Frequency")
      
      ggplot(data_long, aes(x = Row, y = Frequency, fill = Column)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        labs(title = "جدول توافقی - توزیع فراوانی",
             x = "ردیف", y = "فراوانی", fill = "ستون") +
        theme_minimal() +
        theme(text = element_text(family = "Tahoma"))
    })
    
    output$chi2_results <- renderPrint({
      # ایجاد ماتریس داده‌ها
      data_matrix <- matrix(c(
        input$chi2_row1_col1, input$chi2_row1_col2,
        input$chi2_row2_col1, input$chi2_row2_col2
      ), nrow = 2, byrow = TRUE)
      
      # انجام آزمون کای-دو
      test_result <- chisq.test(data_matrix)
      
      cat("نتایج آزمون کای-دو:\n")
      cat("جدول مشاهده شده:\n")
      print(data_matrix)
      cat("\nجدول مورد انتظار:\n")
      print(round(test_result$expected, 2))
      cat("\nآماره کای-دو:", round(test_result$statistic, 4), "\n")
      cat("درجه آزادی:", test_result$parameter, "\n")
      cat("p-value:", round(test_result$p.value, 4), "\n")
      
      # بررسی پیش‌فرض‌ها
      expected <- test_result$expected
      prop_low <- sum(expected < 5) / length(expected)
      
      cat("\nبررسی پیش‌فرض‌ها:\n")
      cat("تعداد خانه‌ها با فراوانی مورد انتظار <5:", sum(expected < 5), "\n")
      cat("درصد خانه‌ها با فراوانی مورد انتظار <5:", round(prop_low * 100, 1), "%\n")
      
      if (prop_low > 0.2) {
        cat("هشدار: بیش از 20% خانه‌ها فراوانی مورد انتظار کمتر از 5 دارند!\n")
        cat("پیشنهاد: از آزمون فیشر استفاده کنید\n")
      }
      
      if (test_result$p.value < as.numeric(input$chi2_alpha)) {
        cat("\nنتیجه: رد فرض صفر - رابطه معنی‌دار بین متغیرها وجود دارد\n")
      } else {
        cat("\nنتیجه: عدم رد فرض صفر - رابطه معنی‌دار بین متغیرها وجود ندارد\n")
      }
    })
  })
  
  # توابع مشابه برای بخش‌های دیگر...
  
  # خلاصه آزمون‌های چندگروهی
  output$multi_group_summary_table <- renderTable({
    data.frame(
      "آزمون" = c("ANOVA", "کراسکال-والیس", "توكی", "دان"),
      "نوع" = c("پارامتری", "ناپارامتری", "تعقیبی", "تعقیبی"),
      "هدف" = c(
        "مقایسه میانگین چند گروه",
        "مقایسه توزیع چند گروه", 
        "مقایسه جفت گروه‌ها پس از ANOVA",
        "مقایسه جفت گروه‌ها پس از کراسکال-والیس"
      ),
      "پیش‌فرض" = c(
        "نرمال بودن، همسانی واریانس",
        "عدم نیاز به نرمال بودن",
        "معنی‌دار بودن ANOVA",
        "معنی‌دار بودن کراسکال-والیس"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # خلاصه آزمون‌های مکرر
  output$repeated_measures_summary_table <- renderTable({
    data.frame(
      "آزمون" = c("Repeated Measures ANOVA", "فریدمن", "کوکران"),
      "نوع داده" = c("کمی نرمال", "کمی/ترتیبی", "دوحالتی"),
      "طرح مطالعه" = c("اندازه‌گیری مکرر", "اندازه‌گیری مکرر", "اندازه‌گیری مکرر"),
      "پیش‌فرض" = c("نرمال بودن، کروی بودن", "عدم نیاز به نرمال بودن", "داده‌های دوحالتی")
    )
  }, striped = TRUE, hover = TRUE)
  
  # جدول تفسیر همبستگی
  output$correlation_interpretation_table <- renderTable({
    data.frame(
      "مقدار ضریب" = c("0.9 - 1.0", "0.7 - 0.9", "0.5 - 0.7", "0.3 - 0.5", "0.0 - 0.3"),
      "قدرت رابطه" = c("خیلی قوی", "قوی", "متوسط", "ضعیف", "بسیار ضعیف"),
      "تفسیر" = c(
        "رابطه تقریباً کامل",
        "رابطه قوی و معنادار",
        "رابطه متوسط و قابل توجه",
        "رابطه ضعیف اما معمولاً معنادار",
        "رابطه ناچیز یا بدون رابطه"
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  # راهنمای انتخاب آزمون همبستگی
  output$correlation_selection_guide <- renderTable({
    data.frame(
      "نوع داده" = c("کمی نرمال", "کمی غیرنرمال", "ترتیبی", "داده‌های رتبه‌ای"),
      "آزمون" = c("پیرسون", "اسپیرمن", "اسپیرمن", "کندال"),
      "فرضیه" = c(
        "رابطه خطی بین دو متغیر نرمال",
        "رابطه یکنوا بین دو متغیر",
        "رابطه بین دو متغیر ترتیبی", 
        "هماهنگی بین رتبه‌ها"
      ),
      "مقاومت به پرت" = c("کم", "متوسط", "متوسط", "زیاد")
    )
  }, striped = TRUE, hover = TRUE)
  
  # جدول تفسیر OR
  output$or_interpretation_table <- renderTable({
    data.frame(
      "مقدار OR" = c("OR > 3", "1.5 < OR ≤ 3", "1.2 < OR ≤ 1.5", "0.8 < OR ≤ 1.2", "OR ≤ 0.8"),
      "تفسیر" = c("خطر بسیار زیاد", "خطر زیاد", "خطر متوسط", "بدون اثر معنادار", "اثر محافظتی"),
      "اهمیت بالینی" = c("بسیار مهم", "مهم", "متوسط", "ناچیز", "مهم")
    )
  }, striped = TRUE, hover = TRUE)
  
  # خلاصه روش‌های تحلیل بقا
  output$survival_methods_table <- renderTable({
    data.frame(
      "روش" = c("کاپلان-مایر", "لگرانک", "کاکس"),
      "نوع" = c("غیرپارامتری", "نموداری", "نیمه‌پارامتری"),
      "هدف" = c(
        "تخمین تابع بقا",
        "مقایسه منحنی‌های بقا",
        "مدل‌سازی عوامل مؤثر بر بقا"
      ),
      "خروجی" = c("منحنی بقا", "p-value", "نسبت خطر")
    )
  }, striped = TRUE, hover = TRUE)
  
  # توابع output$ برای فصل‌های ۱۶ به بعد
  
  # فصل ۱۶: نکات کاربردی
  output$sample_size_table <- renderTable({
    data.frame(
      "نوع مطالعه" = c("توصیفی", "تحلیلی - دو گروه", "تحلیلی - چند گروه", "همبستگی", "رگرسیون"),
      "حداقل حجم نمونه" = c("100", "30 در هر گروه", "20 در هر گروه", "50", "10-15 به ازای هر متغیر"),
      "روش محاسبه" = c("قاعده کلی", "توان آماری", "ANOVA", "ضریب همبستگی", "R²"),
      "نرم‌افزار" = c("G*Power", "G*Power", "G*Power", "G*Power", "G*Power")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  output$software_comparison_table <- renderTable({
    data.frame(
      "نرم‌افزار" = c("SPSS", "R", "SAS", "Stata", "Python"),
      "هزینه" = c("پولی", "رایگان", "گران", "پولی", "رایگان"),
      "سختی یادگیری" = c("آسان", "متوسط", "سخت", "آسان", "متوسط"),
      "قدرت تحلیل" = c("متوسط", "عالی", "عالی", "خوب", "عالی"),
      "کاربرد در پزشکی" = c("زیاد", "زیاد", "زیاد", "متوسط", "در حال رشد")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  # توابع برای فصل ۱۱: همبستگی
  output$correlation_interpretation_table <- renderTable({
    data.frame(
      "مقدار ضریب" = c("0.8 تا 1.0", "0.6 تا 0.8", "0.4 تا 0.6", "0.2 تا 0.4", "0.0 تا 0.2"),
      "تفسیر" = c("همبستگی بسیار قوی", "همبستگی قوی", "همبستگی متوسط", "همبستگی ضعیف", "همبستگی بسیار ضعیف"),
      "مثال پزشکی" = c("سن و فشار خون", "وزن و BMI", "قد و وزن", "سن و سطح ویتامین D", "قد و فشار خون")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  output$correlation_selection_guide <- renderTable({
    data.frame(
      "نوع داده" = c("کمی نرمال", "کمی غیرنرمال", "ترتیبی", "اسمی"),
      "آزمون مناسب" = c("پیرسون", "اسپیرمن", "کندال", "کرامرز V"),
      "پیش‌فرض‌ها" = c("نرمال بودن، خطی بودن", "ندارد", "ندارد", "ندارد"),
      "مثال" = c("فشار خون و سن", "سطح درد و سن", "درجه سرطان و رضایت", "گروه خونی و جنسیت")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  # توابع برای فصل ۱۲: رگرسیون خطی
  output$slr_plot <- renderPlot({
    if (input$run_slr == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$slr_sample_size
      x <- rnorm(n, 50, 10)
      y <- input$slr_intercept + input$slr_slope * x + rnorm(n, 0, input$slr_noise * 5)
      
      model <- lm(y ~ x)
      predictions <- predict(model)
      
      ggplot(data.frame(x, y), aes(x = x, y = y)) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        labs(title = "رگرسیون خطی ساده",
             x = "متغیر مستقل (X)",
             y = "متغیر وابسته (Y)") +
        theme_minimal() +
        theme(text = element_text(family = "Tahoma"))
    })
  })
  
  output$slr_results <- renderPrint({
    if (input$run_slr == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$slr_sample_size
      x <- rnorm(n, 50, 10)
      y <- input$slr_intercept + input$slr_slope * x + rnorm(n, 0, input$slr_noise * 5)
      
      model <- lm(y ~ x)
      cat("نتایج رگرسیون خطی ساده:\n\n")
      print(summary(model))
      cat("\nضرایب استاندارد شده:\n")
      print(lm.beta::lm.beta(model))
    })
  })
  
  output$mlr_plot <- renderPlot({
    if (input$run_mlr == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$mlr_sample_size
      
      # ایجاد داده‌های با همبستگی مشخص
      sigma <- matrix(c(1, input$mlr_correlation, 
                        input$mlr_correlation, 1), ncol = 2)
      x_data <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = sigma)
      
      x1 <- x_data[, 1]
      x2 <- x_data[, 2]
      y <- input$mlr_intercept + input$mlr_beta1 * x1 + input$mlr_beta2 * x2 + rnorm(n, 0, 5)
      
      # نمودارهای پراکندگی
      p1 <- ggplot(data.frame(x1, y), aes(x = x1, y = y)) +
        geom_point(alpha = 0.6, color = "blue") +
        geom_smooth(method = "lm", color = "red") +
        labs(x = "X1", y = "Y") +
        theme_minimal()
      
      p2 <- ggplot(data.frame(x2, y), aes(x = x2, y = y)) +
        geom_point(alpha = 0.6, color = "green") +
        geom_smooth(method = "lm", color = "red") +
        labs(x = "X2", y = "Y") +
        theme_minimal()
      
      grid.arrange(p1, p2, ncol = 2)
    })
  })
  
  output$mlr_results <- renderPrint({
    if (input$run_mlr == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$mlr_sample_size
      
      sigma <- matrix(c(1, input$mlr_correlation, 
                        input$mlr_correlation, 1), ncol = 2)
      x_data <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = sigma)
      
      x1 <- x_data[, 1]
      x2 <- x_data[, 2]
      y <- input$mlr_intercept + input$mlr_beta1 * x1 + input$mlr_beta2 * x2 + rnorm(n, 0, 5)
      
      model <- lm(y ~ x1 + x2)
      cat("نتایج رگرسیون خطی چندگانه:\n\n")
      print(summary(model))
      cat("\nماتریس همبستگی:\n")
      cor_matrix <- cor(data.frame(x1, x2, y))
      print(cor_matrix)
    })
  })
  
  # توابع برای فصل ۱۳: رگرسیون لجستیک
  output$logit_plot <- renderPlot({
    if (input$run_logit == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$logit_sample_size
      x <- rnorm(n, 0, 1)
      
      # محاسبه احتمال با استفاده از تابع لجستیک
      linear_combination <- input$logit_intercept + input$logit_beta * x
      probability <- 1 / (1 + exp(-linear_combination))
      
      # تولید داده‌های باینری
      y <- rbinom(n, 1, probability)
      
      # مدل رگرسیون لجستیک
      model <- glm(y ~ x, family = binomial)
      predicted_probs <- predict(model, type = "response")
      
      # نمودار
      df <- data.frame(x, y, predicted_probs)
      df <- df[order(df$x), ]
      
      ggplot(df, aes(x = x)) +
        geom_point(aes(y = y, color = as.factor(y)), alpha = 0.6) +
        geom_line(aes(y = predicted_probs), color = "red", size = 1) +
        scale_color_manual(values = c("blue", "green"), 
                           labels = c("عدم رخداد", "رخداد")) +
        labs(title = "رگرسیون لجستیک",
             x = "متغیر مستقل",
             y = "احتمال رخداد",
             color = "وضعیت واقعی") +
        theme_minimal() +
        theme(text = element_text(family = "Tahoma"))
    })
  })
  
  output$logit_results <- renderPrint({
    if (input$run_logit == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$logit_sample_size
      x <- rnorm(n, 0, 1)
      
      linear_combination <- input$logit_intercept + input$logit_beta * x
      probability <- 1 / (1 + exp(-linear_combination))
      y <- rbinom(n, 1, probability)
      
      model <- glm(y ~ x, family = binomial)
      
      cat("نتایج رگرسیون لجستیک:\n\n")
      print(summary(model))
      
      cat("\nنسبت شانس (Odds Ratio):\n")
      or <- exp(coef(model))
      ci <- exp(confint(model))
      results <- data.frame(
        "متغیر" = names(or),
        "OR" = round(or, 3),
        "CI 2.5%" = round(ci[,1], 3),
        "CI 97.5%" = round(ci[,2], 3)
      )
      print(results)
    })
  })
  
  output$or_interpretation_table <- renderTable({
    data.frame(
      "مقدار OR" = c("> 10", "3-10", "2-3", "1-2", "1", "0.5-1", "0.3-0.5", "0.1-0.3", "< 0.1"),
      "تفسیر" = c("خطر بسیار زیاد", "خطر زیاد", "خطر متوسط", "خطر کم", "بدون اثر", "حفاظت کم", "حفاظت متوسط", "حفاظت زیاد", "حفاظت بسیار زیاد"),
      "مثال پزشکی" = c("سیگار و سرطان ریه", "دیابت و بیماری قلبی", "چاقی و فشار خون", "سن و پوکی استخوان", "گروه خونی و سرطان", "ورزش و دیابت", "ورزش و بیماری قلبی", "واکسن و بیماری", "واکسن و بیماری شدید")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  # توابع برای فصل ۱۴: رگرسیون شمارشی
  output$poisson_plot <- renderPlot({
    if (input$run_poisson == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$poisson_sample_size
      x <- rnorm(n, 0, 1)
      
      # تولید داده‌های پواسون
      lambda <- exp(input$poisson_beta * x + log(input$poisson_lambda))
      y <- rpois(n, lambda)
      
      # اگر پراکندگی بیش از حد وجود دارد
      if (input$poisson_overdispersion > 1) {
        y <- rnbinom(n, size = 1/input$poisson_overdispersion, mu = lambda)
      }
      
      # مدل رگرسیون پواسون
      model <- glm(y ~ x, family = poisson)
      predicted <- predict(model, type = "response")
      
      df <- data.frame(x, y, predicted)
      df <- df[order(df$x), ]
      
      ggplot(df, aes(x = x)) +
        geom_point(aes(y = y), alpha = 0.6, color = "blue") +
        geom_line(aes(y = predicted), color = "red", size = 1) +
        labs(title = "رگرسیون پواسون",
             x = "متغیر مستقل",
             y = "تعداد رویدادها") +
        theme_minimal() +
        theme(text = element_text(family = "Tahoma"))
    })
  })
  
  output$poisson_results <- renderPrint({
    if (input$run_poisson == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$poisson_sample_size
      x <- rnorm(n, 0, 1)
      
      lambda <- exp(input$poisson_beta * x + log(input$poisson_lambda))
      y <- rpois(n, lambda)
      
      if (input$poisson_overdispersion > 1) {
        y <- rnbinom(n, size = 1/input$poisson_overdispersion, mu = lambda)
      }
      
      model_poisson <- glm(y ~ x, family = poisson)
      
      cat("نتایج رگرسیون پواسون:\n\n")
      print(summary(model_poisson))
      
      # بررسی پراکندگی بیش از حد
      cat("\nبررسی پراکندگی بیش از حد:\n")
      dispersion <- sum(residuals(model_poisson, type = "pearson")^2) / model_poisson$df.residual
      cat("آماره پراکندگی:", round(dispersion, 3), "\n")
      
      if (dispersion > 1.5) {
        cat("پراکندگی بیش از حد وجود دارد. استفاده از رگرسیون دوجمله‌ای منفی توصیه می‌شود.\n")
      } else {
        cat("پراکندگی بیش از حد وجود ندارد. مدل پواسون مناسب است.\n")
      }
    })
  })
  
  # توابع برای فصل ۱۵: تحلیل بقا
  output$km_plot <- renderPlot({
    if (input$run_km == 0) return()
    
    isolate({
      set.seed(123)
      n_groups <- input$km_n_groups
      n_per_group <- input$km_sample_size
      censoring_rate <- input$km_censoring / 100
      
      # ایجاد داده‌های بقا
      survival_data <- data.frame()
      group_colors <- c("red", "blue", "green", "purple")
      group_names <- c("گروه ۱", "گروه ۲", "گروه ۳", "گروه ۴")
      
      for (i in 1:n_groups) {
        # زمان بقا
        mean_survival <- switch(i,
                                input$km_group1_mean,
                                input$km_group2_mean,
                                input$km_group3_mean,
                                input$km_group4_mean)
        
        time <- rexp(n_per_group, rate = 1/mean_survival)
        
        # سانسورشدگی
        censored <- runif(n_per_group) < censoring_rate
        status <- as.numeric(!censored)
        
        # برای بیماران سانسور شده، زمان مشاهده را کوتاه‌تر می‌کنیم
        time[censored] <- time[censored] * runif(sum(censored), 0.1, 0.8)
        
        group_data <- data.frame(
          time = time,
          status = status,
          group = factor(rep(group_names[i], n_per_group))
        )
        
        survival_data <- rbind(survival_data, group_data)
      }
      
      # محاسبه منحنی کاپلان-مایر
      km_fit <- survfit(Surv(time, status) ~ group, data = survival_data)
      
      # رسم منحنی بقا
      ggsurvplot(km_fit, data = survival_data,
                 palette = group_colors[1:n_groups],
                 conf.int = TRUE,
                 risk.table = TRUE,
                 pval = TRUE,
                 legend.title = "گروه‌ها",
                 legend.labs = group_names[1:n_groups],
                 xlab = "زمان (ماه)",
                 ylab = "احتمال بقا",
                 title = "منحنی بقای کاپلان-مایر")$plot +
        theme(text = element_text(family = "Tahoma"))
    })
  })
  
  output$km_results <- renderPrint({
    if (input$run_km == 0) return()
    
    isolate({
      set.seed(123)
      n_groups <- input$km_n_groups
      n_per_group <- input$km_sample_size
      censoring_rate <- input$km_censoring / 100
      
      survival_data <- data.frame()
      group_names <- c("گروه ۱", "گروه ۲", "گروه ۳", "گروه ۴")
      
      for (i in 1:n_groups) {
        mean_survival <- switch(i,
                                input$km_group1_mean,
                                input$km_group2_mean,
                                input$km_group3_mean,
                                input$km_group4_mean)
        
        time <- rexp(n_per_group, rate = 1/mean_survival)
        censored <- runif(n_per_group) < censoring_rate
        status <- as.numeric(!censored)
        time[censored] <- time[censored] * runif(sum(censored), 0.1, 0.8)
        
        group_data <- data.frame(
          time = time,
          status = status,
          group = factor(rep(group_names[i], n_per_group))
        )
        
        survival_data <- rbind(survival_data, group_data)
      }
      
      km_fit <- survfit(Surv(time, status) ~ group, data = survival_data)
      
      cat("نتایج تحلیل بقا:\n\n")
      cat("خلاصه منحنی‌های بقا:\n")
      print(summary(km_fit))
      
      cat("\nآزمون لگرانک برای مقایسه گروه‌ها:\n")
      logrank_test <- survdiff(Surv(time, status) ~ group, data = survival_data)
      print(logrank_test)
    })
  })
  
  output$cox_plot <- renderPlot({
    if (input$run_cox == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$cox_sample_size
      
      # ایجاد داده‌های مصنوعی
      age <- rnorm(n, 60, 10)
      treatment <- sample(0:1, n, replace = TRUE)
      stage <- sample(1:4, n, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2))
      
      # محاسبه تابع خطر
      hazard_ratio <- input$cox_hr_age^((age - 60)/10) * 
        input$cox_hr_treatment^treatment * 
        input$cox_hr_stage^(stage - 1)
      
      # زمان بقا
      baseline_hazard <- 0.01
      time <- rexp(n, rate = baseline_hazard * hazard_ratio)
      
      # سانسورشدگی
      censored <- runif(n) < 0.2
      status <- as.numeric(!censored)
      
      survival_data <- data.frame(
        time = time,
        status = status,
        age = age,
        treatment = factor(treatment, levels = c(0, 1), labels = c("درمان قدیم", "درمان جدید")),
        stage = factor(stage)
      )
      
      # مدل کاکس
      cox_model <- coxph(Surv(time, status) ~ age + treatment + stage, data = survival_data)
      
      # نمودار نسبت خطر
      forest_data <- broom::tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
      
      ggplot(forest_data, aes(x = estimate, y = term)) +
        geom_point(size = 3) +
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
        geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
        scale_x_log10() +
        labs(title = "نمودار جنگلی نسبت‌های خطر",
             x = "نسبت خطر (HR)",
             y = "متغیرها") +
        theme_minimal() +
        theme(text = element_text(family = "Tahoma"))
    })
  })
  
  output$cox_results <- renderPrint({
    if (input$run_cox == 0) return()
    
    isolate({
      set.seed(123)
      n <- input$cox_sample_size
      
      age <- rnorm(n, 60, 10)
      treatment <- sample(0:1, n, replace = TRUE)
      stage <- sample(1:4, n, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.2))
      
      hazard_ratio <- input$cox_hr_age^((age - 60)/10) * 
        input$cox_hr_treatment^treatment * 
        input$cox_hr_stage^(stage - 1)
      
      baseline_hazard <- 0.01
      time <- rexp(n, rate = baseline_hazard * hazard_ratio)
      censored <- runif(n) < 0.2
      status <- as.numeric(!censored)
      
      survival_data <- data.frame(
        time = time,
        status = status,
        age = age,
        treatment = factor(treatment, levels = c(0, 1), labels = c("درمان قدیم", "درمان جدید")),
        stage = factor(stage)
      )
      
      cox_model <- coxph(Surv(time, status) ~ age + treatment + stage, data = survival_data)
      
      cat("نتایج رگرسیون کاکس:\n\n")
      print(summary(cox_model))
      
      cat("\nبررسی پیش‌فرض خطرات متناسب:\n")
      ph_test <- cox.zph(cox_model)
      print(ph_test)
    })
  })
  
  output$survival_methods_table <- renderTable({
    data.frame(
      "روش" = c("کاپلان-مایر", "لگرانک", "کاکس", "مدل‌های پارامتریک", "مدل‌های رقابتی"),
      "نوع" = c("ناپارامتری", "ناپارامتری", "نیمه پارامتری", "پارامتری", "مختلف"),
      "کاربرد" = c("تخمین تابع بقا", "مقایسه گروه‌ها", "بررسی عوامل خطر", "مدل‌سازی دقیق", "رویدادهای رقابتی"),
      "مزایا" = c("ساده، بدون فرض", "مقایسه گروه‌ها", "کنترل متغیرها", "دقت بالا", "واقع‌بینانه"),
      "معایب" = c("عدم کنترل متغیرها", "عدم کنترل متغیرها", "پیش‌فرض خطرات متناسب", "فرض توزیع", "پیچیدگی")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')
  
  # جداول راهنمای انتخاب آزمون
  output$quantitative_tests_guide <- renderTable({
    data.frame(
      "نوع داده" = c("نرمال", "غیرنرمال", "نرمال", "غیرنرمال", "نرمال", "غیرنرمال"),
      "تعداد گروه‌ها" = c("یک گروه", "یک گروه", "دو گروه مستقل", "دو گروه مستقل", "دو گروه وابسته", "دو گروه وابسته"),
      "آزمون" = c("t تک نمونه‌ای", "ویلکاکسون", "t مستقل", "من-ویتنی", "t زوجی", "ویلکاکسون زوجی")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$qualitative_tests_guide <- renderTable({
    data.frame(
      "نوع داده" = c("اسمی", "اسمی", "ترتیبی", "ترتیبی", "دوحالتی وابسته"),
      "تعداد گروه‌ها" = c("دو گروه", "چند گروه", "دو گروه", "چند گروه", "دو اندازه‌گیری"),
      "آزمون" = c("کای-دو یا فیشر", "کای-دو", "من-ویتنی", "کراسکال-والیس", "مک نمار")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # محاسبه حجم نمونه
  observeEvent(input$calc_sample_size, {
    output$sample_size_result <- renderPrint({
      # محاسبه ساده حجم نمونه برای t-test
      power <- input$power
      alpha <- input$alpha
      effect_size <- input$effect_size
      
      # استفاده از فرمول ساده
      n <- ceiling(16 / (effect_size^2))
      
      cat("نتایج محاسبه حجم نمونه:\n")
      cat("توان آماری:", power, "\n")
      cat("سطح معنی‌داری:", alpha, "\n")
      cat("اندازه اثر:", effect_size, "\n")
      cat("حجم نمونه مورد نیاز برای هر گروه:", n, "\n")
      cat("حجم نمونه کل (برای دو گروه):", n * 2, "\n")
      
      # تفسیر
      cat("\nتفسیر:\n")
      if (effect_size <= 0.2) {
        cat("اندازه اثر کوچک - نیاز به حجم نمونه بزرگ\n")
      } else if (effect_size <= 0.5) {
        cat("اندازه اثر متوسط - حجم نمونه متعادل\n")
      } else {
        cat("اندازه اثر بزرگ - حجم نمونه کوچک\n")
      }
    })
  })
  
  # محاسبه توان آماری
  observeEvent(input$calc_power, {
    output$power_result <- renderPrint({
      n <- input$sample_size_power
      alpha <- input$alpha_power
      effect_size <- input$effect_size_power
      
      # محاسبه ساده توان
      power <- pnorm(sqrt(n * effect_size^2 / 4) - qnorm(1 - alpha/2))
      
      cat("نتایج محاسبه توان آماری:\n")
      cat("حجم نمونه:", n, "\n")
      cat("سطح معنی‌داری:", alpha, "\n")
      cat("اندازه اثر:", effect_size, "\n")
      cat("توان آماری:", round(power, 3), "\n")
      
      # تفسیر
      cat("\nتفسیر:\n")
      if (power >= 0.8) {
        cat("✅ توان کافی - مطالعه قابل اتکا\n")
      } else if (power >= 0.6) {
        cat("⚠️ توان متوسط - نیاز به احتیاط در تفسیر\n")
      } else {
        cat("❌ توان ناکافی - خطر خطای نوع دوم بالا\n")
      }
    })
  })
  
  # تبدیل اندازه اثر
  observeEvent(input$convert_effect, {
    output$effect_conversion_result <- renderPrint({
      effect_value <- input$effect_value
      effect_type <- input$effect_type
      
      cat("نتایج تبدیل اندازه اثر:\n")
      cat("نوع اندازه اثر:", effect_type, "\n")
      cat("مقدار:", effect_value, "\n\n")
      
      if (effect_type == "d کوهن") {
        r <- effect_value / sqrt(effect_value^2 + 4)
        eta2 <- effect_value^2 / (effect_value^2 + 4)
        cat("r:", round(r, 3), "\n")
        cat("η²:", round(eta2, 3), "\n")
        cat("φ:", round(sqrt(eta2), 3), "\n")
      } else if (effect_type == "r") {
        d <- (2 * effect_value) / sqrt(1 - effect_value^2)
        eta2 <- effect_value^2
        cat("d کوهن:", round(d, 3), "\n")
        cat("η²:", round(eta2, 3), "\n")
        cat("φ:", round(effect_value, 3), "\n")
      } else if (effect_type == "η²") {
        d <- 2 * sqrt(effect_value / (1 - effect_value))
        r <- sqrt(effect_value)
        cat("d کوهن:", round(d, 3), "\n")
        cat("r:", round(r, 3), "\n")
        cat("φ:", round(sqrt(effect_value), 3), "\n")
      }
      
      cat("\nراهنمای تفسیر:\n")
      if (effect_value <= 0.1) cat("اثر بسیار کوچک\n")
      else if (effect_value <= 0.3) cat("اثر کوچک\n")
      else if (effect_value <= 0.5) cat("اثر متوسط\n")
      else cat("اثر بزرگ\n")
    })
  })
  
  # جدول حجم نمونه پیشنهادی
  output$sample_size_table <- renderTable({
    data.frame(
      "اندازه اثر" = c("بسیار کوچک (0.01)", "کوچک (0.2)", "متوسط (0.5)", "بزرگ (0.8)"),
      "حجم نمونه هر گروه" = c("> 1000", "~ 400", "~ 64", "~ 25"),
      "توان (α=0.05)" = c("80%", "80%", "80%", "80%"),
      "کاربرد" = c("مطالعات اپیدمیولوژیک", "تحقیقات بالینی", "آزمایشگاه", "پایلوت")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # چک‌لیست تعاملی
  output$checklist_table <- renderTable({
    data.frame(
      "مرحله" = c("طراحی", "جمع‌آوری داده", "تحلیل", "گزارش"),
      "اقدامات کلیدی" = c(
        "تعیین حجم نمونه، انتخاب آزمون",
        "کنترل کیفیت، مدیریت missing data",
        "بررسی پیش‌فرض‌ها، محاسبه اندازه اثر",
        "گزارش شفاف، تفسیر بالینی"
      ),
      "خروجی" = c(
        "پروتکل مطالعه",
        "داده‌های تمیز",
        "نتایج تحلیل",
        "مقاله/گزارش"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
}


# تابع برای بهینه‌سازی بیشتر
optimize_app <- function() {
  # غیرفعال کردن پیام‌های اضافی
  options(shiny.sanitize.errors = TRUE)
  options(shiny.trace = FALSE)
  
  # تنظیمات حافظه
  options(shiny.fullstacktrace = FALSE)
  options(shiny.autoreload = FALSE)
}

# اجرای برنامه
optimize_app()
shinyApp(ui = ui, server = server)
