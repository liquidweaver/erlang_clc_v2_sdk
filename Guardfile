def which(cmd)
  exts = ENV['PATHEXT'] ? ENV['PATHEXT'].split(';') : ['']
  ENV['PATH'].split(File::PATH_SEPARATOR).each do |path|
    exts.each { |ext|
      exe = "#{path}/#{cmd}#{ext}"
      return exe if File.executable? exe
    }
  end
  return nil
end

rebar = 'rebar/rebar'

guard :shell do
  notification :terminal_notifier, activate: 'com.googlecode.iterm2'
  app = File.basename(Dir.pwd)
  watch(%r{^(.*/)?(?:src|test)/([^.].*?)(?:_tests?)?.erl$}) do |matches|
    filepath, subdir, modname = matches
    suite = "#{modname}_tests"

    test_module = File.join 'test', "#{suite}.erl"
    test_module = File.join subdir, test_module if subdir

    if Dir[test_module].empty?
      n "no tests for #{modname}", app, :warning
    else
      apps = "apps=#{File.basename(subdir)}" if subdir
      cmd = "#{rebar} -r eunit skip_deps=true #{apps} suites=#{suite}"
      # puts cmd
      output = `#{cmd}`
      if $?.success?
        n "eunit passed for #{suite}", app, :success
      else
        begin
          if logicMatch = /\/([\w\.]+):(\d+):\s*(.*)$/.match(output)
            file, line, reason = logicMatch.captures
            n "#{reason}", "#{file}:#{line}", :failed
            puts "#{file}:#{line}\n#{reason}"
          elsif syntaxMatch = /\n\s*([^\n]+?): ([^\n]+?)(?:...\*failed\*).*?(?:line) (\d+).*?(?:\*\*(?:error|throw):)(.*?)\n\n/m.match(output)
            suite, test, line, details = syntaxMatch.captures
            n "#{test}", "#{suite}:#{line}", :failed
            puts output
          else
            throw new exception
          end
        rescue
          n "Yikes, Guard had some problems...", app, :warning
          puts output
        end
      end
      nil
    end

  end
end