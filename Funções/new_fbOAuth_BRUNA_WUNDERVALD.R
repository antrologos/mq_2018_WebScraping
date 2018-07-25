# Esta função foi elaborada por Bruna Wundervald (http://brunaw.com/) e divulgada 
# no dia 22/07/2018 no blog do 'Curso R', por Julio Trecenti e pela própria Bruna.
# Veja o post original em: http://curso-r.com/blog/2018/07/23/2018-07-22-rfacebook/
#
# Trata-se de uma modificação na função fbOAuth do pacote Rfacebook, para realizar
# a conexão do R com o API do Facebook. Os autores descrevem o seguinte:
#
#                       "A API estava recusando acesso às opções user_relationships 
#                        e publish_actions, que são requeridas por meio da função 
#                        Rfacebook::fbOAuth(). Aha! Depois de estudar um pouquinho,
#                        notamos que a API do Facebook mudou recentemente. Isso ocorreu
#                        por conta de alterações na política de privacidade do Facebook,
#                        que é sempre algo polêmico. Hoje em dia não é mais possível 
#                        autorizar aplicativos para terem acesso à rede de relacionamentos
#                        e ações do usuário. O problema é que infelizmente o mantenedor do
#                        Rfacebook parou de atualizar o pacote"
#        
# A solução proposta é bastante simples, mas muito engenhosa e perspicaz (o melhor tipo de 
# solução, afinal). Uma única linha de comando foi alterada no código original. Ela está 
# marcada como comentário no corpo do código.
#
# Façam bom proveito!


new_fbOAuth <- function (app_id, app_secret, extended_permissions = FALSE, 
                         legacy_permissions = FALSE, scope = NULL) {
        facebook <- oauth_endpoint(authorize = "https://www.facebook.com/dialog/oauth", 
                                   access = "https://graph.facebook.com/oauth/access_token")
        myapp <- oauth_app("facebook", app_id, app_secret)
        if (is.null(scope)) {
                if (extended_permissions == TRUE) {
                        
                        
                        # as alterações significativas estão aqui >>>
                        
                        scope <- c("user_birthday", "user_hometown", "user_location", 
                                   # "user_relationships", "publish_actions", 
                                   "user_status", "user_likes")
                        
                        
                } else {
                        scope <- c("public_profile", "user_friends")
                }
                if (legacy_permissions == TRUE) {
                        scope <- c(scope, "read_stream")
                }
        }
        if (packageVersion("httr") < "1.2") {
                stop("Rfacebook requires httr version 1.2.0 or greater")
        }
        if (packageVersion("httr") <= "0.2") {
                facebook_token <- oauth2.0_token(facebook, myapp, scope = scope)
                fb_oauth <- sign_oauth2.0(facebook_token$access_token)
                if (GET("https://graph.facebook.com/me", config = fb_oauth)$status == 200) {
                        message("Authentication successful.")
                }
        }
        if (packageVersion("httr") > "0.2" & packageVersion("httr") <= 
            "0.6.1") {
                fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope, cache = FALSE)
                if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status == 200) {
                        message("Authentication successful.")
                }
        }
        if (packageVersion("httr") > "0.6.1" & packageVersion("httr") < 
            "1.2") {
                Sys.setenv(HTTR_SERVER_PORT = "1410/")
                fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope, 
                                           cache = FALSE)
                if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status == 200) {
                        message("Authentication successful.")
                }
        }
        if (packageVersion("httr") >= "1.2") {
                fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope, 
                                           cache = FALSE)
                if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status == 200) {
                        message("Authentication successful.")
                }
        }
        error <- tryCatch(callAPI("https://graph.facebook.com/pablobarbera", 
                                  fb_oauth), error = function(e) e)
        if (inherits(error, "error")) {
                class(fb_oauth)[4] <- "v2"
        }
        if (!inherits(error, "error")) {
                class(fb_oauth)[4] <- "v1"
        }
        return(fb_oauth)
} 