Rails.application.routes.draw do
  mount ActionCable.server => '/cable'
  get '/ws' => 'ws_debugger#show'
end
