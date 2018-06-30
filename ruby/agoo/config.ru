# frozen_string_literal: true

require 'shooter'

# A minimal startup of the Agoo rack handle using rackup. Note this does not
# allow for loading any static assets.
# $ bundle exec rackup -r agoo -s agoo

# Make requests on port 9292 to received responses.

run Shooter
