c = File.read('corners.txt')
open('c.dot', 'w') do |file|
  file.puts "digraph G {"
  cluster = {}
  c.lines.map { |l| l.strip.split("\t") }.combination(2).each do |a|
    l = a[0]; l1 = l[1]
    r = a[1]; r1 = r[1]
    cluster[l1] ||= [l1]
    if l[1].tr("5678", "6785") == r[1] then
      cluster[l1] << r1
    end
    if l[1].tr("5678", "7856") == r[1] then
      cluster[l1] << r1
    end
    if l[1].tr("5678", "8567") == r[1] then
      cluster[l1] << r1
    end
    if l[1].tr("3287", "8732") == r[1] then
      cluster[l1] << r1
    end
    if l[1].tr("3467", "6734") == r[1] then
      cluster[l1] << r1
    end
  end
  cluster.each do |l, r|
    if r.length > 1
      file.puts <<EOT
subgraph cluster_#{l} {
  #{r.map{|a| "#{a};\n"}.join}
}
EOT
    end
  end
  c.lines.map { |l| l.strip.split("\t") }.combination(2).each do |a|
    if (a[0][2].length rescue 0) < (a[1][2].length rescue 0)
      l = a[0]; r = a[1]
    else
      l = a[1]; r = a[0]
    end
    if Regexp.new("^#{l[2]} ([UFR][2']?)$") =~ r[2]
      file.puts "#{l[1]} -> #{r[1]} [ label = \"#{$1}\" ];"
    end
  end
  file.puts "}"
end
